{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import                          Control.Lens
import                          Data.Data                        (Proxy(..))
import                          Data.String.Interpolate.IsString (i)
import                          Data.Text                        (Text)
import qualified                Data.Text                        as Text
import                          Data.Time.Clock                  (getCurrentTime)
import                          Data.UUID                        (UUID)
import qualified                Data.UUID                        as UUID
import                          Test.Hspec
import                          Test.Hspec.Wai
import                          Test.Hspec.Wai.JSON

import                          Network.Wai                      (Application)

import qualified "auth-service" Api
import                          Backend
import                          Test.Common
import                          Types

import                          NejlaCommon.Test                 as NC

iid :: UUID
Just iid = UUID.fromString "3afe62f4-7235-4b86-a418-923aaa4a5c28"

runTest :: SpecWith ((), Application) -> IO ()
runTest spec = withTestDB $ \pool -> do
  hspec $ flip around spec $ \f -> do
    withConf Nothing pool $ \conf -> do
      _ <- runAPI pool conf $ addInstance (Just $ InstanceID iid) "instance1"
      f ((), Api.serveAPI pool conf)

main :: IO ()
main = runTest spec

spec :: SpecWith ((), Application)
spec = do
  describe "admin API" adminApiSpec


addUser :: Text -> WaiSession st UUID
addUser name = do
  res <- postJ "/admin/users" [json|{ "name": #{name}
                                    , "email" : #{name <> "@example.com"}
                                    , "password" : "pwd"
                                    , "instances" : [#{iid}]
                                    , "roles": []
                                    } |] `shouldReturnA` (Proxy @Types.ReturnUser)
  return $ res ^. user . _UserID

loginReq :: Text -> Text -> WaiSession st Text
loginReq username password = do
  res <- postJ [i|/login|] [json|{ "user": #{username}
                                 , "password": #{password}
                                }|] `shouldReturnA` (Proxy @Types.ReturnLogin)
  return $ res ^. token . _B64Token



adminApiSpec :: SpecWith ((), Application)
adminApiSpec = do
  describe "/admin/users" $ do
    describe "POST" $ do
      it "returns 200" $ do
        postJ "/admin/users" [json|{ "name": "robert"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   , "roles": []
                                   } |] `shouldRespondWith` 200
      it "allows the user to login" $ do
        postJ "/admin/users" [json|{ "name": "robert"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   , "roles": []
                                   } |] `shouldRespondWith` 200
        -- Check if new user can login
        postJ "/login" [json| { "user": "no@spam.please"
                              , "password": "pwd"
                              } |] `shouldRespondWith` 200
      it "disallows duplicate email addresses" $ do
        postJ "/admin/users" [json|{ "name": "robert"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   , "roles": []
                                   } |] `shouldRespondWith` 200
        postJ "/admin/users" [json|{ "name": "bla"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   , "roles": []
                                   } |] `shouldRespondWith` 409

    describe "GET" $ do
      it "returns the list of users" $ do
        _ <- addUser "peter"
        _ <- addUser "robert"
        res <- get "/admin/users" `shouldReturnA` (Proxy @[Types.ReturnUserInfo])
        res ^.. each . email `NC.shouldBe` ([ "peter@example.com"
                                            , "robert@example.com"])
  describe "/admin/users/<uid>/deactivate" $ do
    describe "now" $ do
      it "prevents a user from logging in" $ do
        uid <- addUser "robert"
        postJ [i|/login|] [json|{ "user": "robert@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 200


        postJ [i|/admin/users/#{uid}/deactivate|] [json|{"deactivate_at": "now"}|]
              `shouldRespondWith` 204

        postJ [i|/login|] [json|{ "user": "robert@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 403
      it "disables existing tokens" $ do
        uid <- addUser "robert"

        tok <- loginReq "robert@example.com" "pwd"

        get [i|/check-token/#{tok}/#{iid}|] `shouldRespondWith` 200

        postJ [i|/admin/users/#{uid}/deactivate|] [json|{"deactivate_at": "now"}|]
              `shouldRespondWith` 204

        get [i|/check-token/#{tok}/#{iid}|]
                  `shouldRespondWith` 403

    describe "time" $ do
      it "prevents a user from logging in" $ do
        uid <- addUser "robert"
        postJ [i|/login|] [json|{ "user": "robert@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 200


        now <- liftIO getCurrentTime
        postJ [i|/admin/users/#{uid}/deactivate|] [json|{"deactivate_at": #{now}}|]
              `shouldRespondWith` 204

        postJ [i|/login|] [json|{ "user": "robert@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 403

    it "doesn't prevent user from logging in when time is in the future" $ do
      uid <- addUser "robert"

      -- The year 2400 seems like a safe choice here
      postJ [i|/admin/users/#{uid}/deactivate|]
            [json|{"deactivate_at": "2400-08-22T11:36:31.973155386Z" }|]
            `shouldRespondWith` 204

      postJ [i|/login|] [json|{ "user": "robert@example.com"
                              , "password": "pwd"
                              }|] `shouldRespondWith` 200
  describe "/admin/users/<uid>/reactivate" $ do
    it "Should allow a user to log in again" $ do
      uid <- addUser "robert"
      postJ [i|/admin/users/#{uid}/deactivate|] [json|{"deactivate_at": "now"}|]
            `shouldRespondWith` 204

      postJ [i|/admin/users/#{uid}/reactivate|] [json|{}|]
            `shouldRespondWith` 204

      postJ [i|/login|] [json|{ "user": "robert@example.com"
                              , "password": "pwd"
                              }|] `shouldRespondWith` 200
