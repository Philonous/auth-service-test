{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import                          Control.Lens
import                          Control.Monad
import                          Data.ByteString                  (ByteString)
import qualified                Data.ByteString.Lazy             as BSL
import                          Data.Data                        (Proxy(..))
import                          Data.String.Interpolate.IsString (i)
import                          Data.Text                        (Text)
import qualified                Data.Text                        as Text
import qualified                Data.Text.Encoding               as Text
import                          Data.Time.Clock                  (getCurrentTime)
import                          Data.UUID                        (UUID)
import qualified                Data.UUID                        as UUID
import                          Test.Hspec
import                          Test.Hspec.Wai
import                          Test.Hspec.Wai.JSON

import                          Network.Wai                      (Application)
import                          Network.Wai.Test                 (SResponse)

import qualified "auth-service" Api
import                          Audit (AuditSource(AuditSourceTest))
import                          Backend
import                          Monad
import                          Test.Common
import                          Types

import                          NejlaCommon.Test                 as NC

iid :: UUID
Just iid = UUID.fromString "3afe62f4-7235-4b86-a418-923aaa4a5c28"

runTest :: SpecWith ((), Application) -> IO ()
runTest spec = withTestDB $ \pool -> do
  hspec $ flip around spec $ \f -> do
    withConf Nothing pool $ \conf -> do
      let apiState = ApiState { apiStateConfig = conf
                              , apiStateAuditSource = AuditSourceTest
                              }
      _ <- runAPI pool apiState $ do
        createUser adminUser
        addInstance (Just $ InstanceID iid) "instance1"
      f ((), Api.serveAPI pool conf)
  where
    adminUser = AddUser { addUserUuid      = Nothing
                        , addUserEmail     = "admin@example.com"
                        , addUserPassword  = "pwd"
                        , addUserName      = "admin"
                        , addUserPhone     = Nothing
                        , addUserInstances = []
                        , addUserRoles     = ["admin"]
                        }
main :: IO ()
main = runTest spec

spec :: SpecWith ((), Application)
spec = do
  describe "admin API" adminApiSpec


exampleUser :: Text -> [Text] -> BSL.ByteString
exampleUser name roles =
                     [json|{ "name": #{name}
                          , "email" : #{name <> "@example.com"}
                          , "password" : "pwd"
                          , "instances" : [#{iid}]
                          , "roles": #{roles}
                          } |]

addUser :: Text -> Text -> [Text] -> WaiSession st UUID
addUser token name roles = do
  res <- postToken token "/admin/users" (exampleUser name roles)
    `shouldReturnA` (Proxy @Types.ReturnUser)
  return $ res ^. user . _UserID

loginReq :: Text -> Text -> WaiSession st Text
loginReq username password = do
  res <- postJ [i|/login|] [json|{ "user": #{username <> "@example.com"}
                                 , "password": #{password}
                                }|] `shouldReturnA` (Proxy @Types.ReturnLogin)
  return $ res ^. token . _B64Token

withAdminToken :: (Text -> WaiSession st b) -> WaiSession st b
withAdminToken f = do
  token <- loginReq "admin" "pwd"
  f token

withRegularToken :: (Text -> WaiSession st b) -> WaiSession st b
withRegularToken f = withAdminToken $ \adm -> do
  _ <- addUser adm "user" []
  tok <- loginReq "user" "pwd"
  f tok

postToken :: Text -> ByteString -> BSL.ByteString -> WaiSession st SResponse
postToken token path body =
  request "POST" path [ ("Content-Type", "application/json")
                      , ("X-Token", Text.encodeUtf8 token)
                      ] body

getToken :: Text -> ByteString -> WaiSession st SResponse
getToken token path = request "GET" path [("X-Token", Text.encodeUtf8 token)] ""

data TestRequestMethod = GET | POST deriving (Show, Eq, Ord)

data TestRequest = TR { trMethod :: TestRequestMethod
                      , trPath :: ByteString
                      -- We don't want to set a body, but unfortunately servant
                      -- tries to parse the request before we get a chance to
                      -- validate the token
                      , trBody :: BSL.ByteString
                      } deriving (Show)

method :: TestRequestMethod -> ByteString
method trm = case trm of
                GET -> "GET"
                POST -> "POST"


runRequest :: Maybe Text -> TestRequest -> WaiSession st SResponse
runRequest mbToken tr =
  let ct = case trMethod tr of
        -- All our POST requests send json data
        POST | not (BSL.null $ trBody tr)  -> [("Content-Type", "application/json")]
             | otherwise -> []
        GET -> []
      th = case mbToken of
             Nothing -> []
             Just token -> [("X-Token", Text.encodeUtf8 token)]
  in request (method $ trMethod tr) (trPath tr) (concat [ct, th]) (trBody tr)

describeRequest :: TestRequest -> Text
describeRequest tr = Text.decodeUtf8 $ method (trMethod tr) <> " " <> trPath tr

adminEndpoints :: [TestRequest]
adminEndpoints =
  [TR POST "/admin/users" (exampleUser "" [])
  ,TR GET  "/admin/users" ""
  ,TR POST [i|/admin/users/#{iid}/deactivate|] [i|{"deactivate_at": "now"}|]
  ,TR POST [i|/admin/users/#{iid}/reactivate|] ""
  ]

adminApiSpec :: SpecWith ((), Application)
adminApiSpec = do
  describe "endpoint authentication" $ do
    forM_ adminEndpoints  $ \endpoint -> do
      describe (Text.unpack $ describeRequest endpoint) $ do
        it "checks that token is set" $
          runRequest Nothing endpoint `shouldRespondWith` 403
        it "checks that user is admin" $ withRegularToken $ \token -> do
          runRequest (Just token) endpoint `shouldRespondWith` 403

  describe "/admin/users" $ do
    describe "POST" $ do
      it "succeeds" $ withAdminToken $ \admin -> do
        postToken admin "/admin/users" [json|{ "name": "robert"
                                            , "email" : "no@spam.please"
                                            , "password" : "pwd"
                                            , "instances" : [#{iid}]
                                            , "roles": []
                                            } |] `shouldRespondWith` 200

      it "allows created user to login" $ withAdminToken $ \admin -> do
        postToken admin "/admin/users" [json|{ "name": "robert"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   , "roles": []
                                   } |] `shouldRespondWith` 200
        -- Check if new user can login
        postToken admin "/login" [json| { "user": "no@spam.please"
                              , "password": "pwd"
                              } |] `shouldRespondWith` 200

      it "disallows duplicate email addresses" $ withAdminToken $ \admin-> do
        postToken admin "/admin/users" [json|{ "name": "robert"
                                            , "email" : "no@spam.please"
                                            , "password" : "pwd"
                                            , "instances" : [#{iid}]
                                            , "roles": []
                                            } |] `shouldRespondWith` 200
        postToken admin "/admin/users" [json|{ "name": "bla"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   , "roles": []
                                   } |] `shouldRespondWith` 409

    describe "GET" $ do
      it "returns the list of users" $ withAdminToken $ \admin-> do
        _ <- addUser admin "peter" []
        _ <- addUser admin "robert" []
        res <- getToken admin "/admin/users"
                `shouldReturnA` (Proxy @[Types.ReturnUserInfo])
        -- "admin@example.com" is always added since we need admin privileges to
        -- run admin endpoints
        res ^.. each . email `NC.shouldBe` ([ "admin@example.com"
                                            , "peter@example.com"
                                            , "robert@example.com"])

  describe "/admin/users/<uid>/deactivate" $ do
    describe "now" $ do
      it "prevents a user from logging in" $ withAdminToken $ \admin-> do
        uid <- addUser admin "robert" []
        postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 200


        postToken admin [i|/admin/users/#{uid}/deactivate|]
                        [json|{"deactivate_at": "now"}|]
              `shouldRespondWith` 204

        postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 403
      it "disables existing tokens" $ withAdminToken $ \admin-> do
        uid <- addUser admin "robert" []

        tok <- loginReq "robert" "pwd"

        getToken admin [i|/check-token/#{tok}/#{iid}|] `shouldRespondWith` 200

        postToken admin [i|/admin/users/#{uid}/deactivate|]
                        [json|{"deactivate_at": "now"}|]
              `shouldRespondWith` 204

        getToken admin [i|/check-token/#{tok}/#{iid}|]
                  `shouldRespondWith` 403

    describe "time" $ do
      it "prevents a user from logging in" $ withAdminToken $ \admin-> do
        uid <- addUser admin "robert" []
        postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 200


        now <- liftIO getCurrentTime
        postToken admin [i|/admin/users/#{uid}/deactivate|]
                        [json|{"deactivate_at": #{now}}|]
              `shouldRespondWith` 204

        postToken admin [i|/login|]
                        [json|{ "user": "robert@example.com"
                              , "password": "pwd"
                              }|] `shouldRespondWith` 403

    it "doesn't prevent user from logging in when time is in the future"
     $ withAdminToken $ \admin-> do
      uid <- addUser admin "robert" []

      -- The year 2400 seems like a safe choice here
      postToken admin [i|/admin/users/#{uid}/deactivate|]
            [json|{"deactivate_at": "2400-08-22T11:36:31.973155386Z" }|]
            `shouldRespondWith` 204

      postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                              , "password": "pwd"
                              }|] `shouldRespondWith` 200
  describe "/admin/users/<uid>/reactivate" $ do
    it "Should allow a user to log in again" $ withAdminToken $ \admin-> do
      uid <- addUser admin "robert" []
      postToken admin [i|/admin/users/#{uid}/deactivate|]
                      [json|{"deactivate_at": "now"}|]
            `shouldRespondWith` 204

      postToken admin [i|/admin/users/#{uid}/reactivate|] [json|{}|]
            `shouldRespondWith` 204

      postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                              , "password": "pwd"
                              }|] `shouldRespondWith` 200
