{-# LANGUAGE OverloadedStrings #-}
-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE FlexibleContexts #-}
module Backend
  (module Backend
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString.Base64 as B64
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock
import qualified Data.UUID.V4 as UUID
import qualified Database.Persist as P
import           Database.Persist.Sql
import           System.Entropy
import           System.IO
import           System.Random
import qualified Twilio
import qualified Twilio.Messages as Twilio

import qualified Persist.Schema as DB
import           Types


debug t = liftIO $ Text.hPutStrLn stderr t

showText :: Show a => a -> Text
showText = Text.pack . show

policy :: BCrypt.HashingPolicy
policy = BCrypt.fastBcryptHashingPolicy { BCrypt.preferredHashCost = 10 }

tokenTimeoutSeconds :: Integer
tokenTimeoutSeconds = 300 -- 5 minutes

hashPassword :: Password -> API (Maybe PasswordHash)
hashPassword (Password pwd) = liftIO $
    fmap PasswordHash <$>
      BCrypt.hashPasswordUsingPolicy policy (Text.encodeUtf8 pwd)

createUser :: AddUser -> API (Maybe ())
createUser usr = do
    uid <- UserID <$> liftIO UUID.nextRandom
    mbHash <- hashPassword $ usr ^. password
    case mbHash of
     Nothing -> return Nothing
     Just hash -> do
       let dbUser = DB.User { DB.userUuid = uid
                            , DB.userName = usr ^. name
                            , DB.userPasswordHash = hash
                            , DB.userEmail = usr ^. email
                            , DB.userPhone = usr ^. phone
                            }
       _ <- runDB $ P.insert dbUser
       return $ Just ()

otpIDChars :: [Char]
otpIDChars = "CDFGHJKLMNPQRSTVWXYZ2345679"

mkRandomString chars len =
    liftIO $ Text.pack <$> replicateM len (selectOne chars)
  where
    selectOne xs = do
        i <- randomRIO (0, length xs - 1)
        return $ xs !! i

mkRandomOTP ::  API Password
mkRandomOTP = do
    len <- getConfig oTPLength
    Password <$> mkRandomString otpIDChars len


sendOTP :: Username -> Phone -> Password -> API ()
sendOTP (Username user) (Phone p) (Password otp) = do
    debug $ mconcat [ "Sending OTP for user " , user
                    , "(", p , ")"
                    , ": " <> otp
                    ]
    twilioConf<- getConfig twilio
    message <- liftIO . Twilio.runTwilio ( twilioConf ^. account
                                         , twilioConf ^. authToken
                                         ) $ do
      let body = Twilio.PostMessage{ Twilio.sendTo = p
                                   , Twilio.sendFrom = twilioConf ^. sourceNumber
                                   , Twilio.sendBody = otp
                                   }

      Twilio.post body
    debug $ "Twilio message created : " <> showText message


tokenChars :: [Char]
tokenChars = concat [ ['a' .. 'z']
                    , ['A' .. 'Z']
                    , ['0' .. '9']
                    ] -- Roughly 6 bit per char


login :: Login -> API (Either LoginError B64Token)
login Login{ loginUser = username
           , loginPassword = pwd
           , loginOtp      = mbOtp
           } = do
    mbUser <- runDB $ P.getBy (DB.UniqueUsername username)
    case mbUser of
     Nothing -> return $ Left LoginErrorFailed
     Just (Entity _ usr) -> do
         let hash = usr ^. DB.passwordHash
             userId = usr ^. DB.uuid
         case checkPassword hash pwd of
           True -> do
               unless (hashUsesPolicy hash) $ rehash userId
               case mbOtp of
                Nothing -> case DB.userPhone usr of
                            -- No phone number for two-factor auth
                            Nothing -> Right <$> createToken userId
                            Just p  -> do
                                createOTP p userId
                                return $ Left LoginErrorOTPRequired
                Just otp -> do
                    otpTime <- fromIntegral . negate <$> getConfig oTPTimeoutSeconds
                    now <- liftIO getCurrentTime
                    let cutoff = otpTime `addUTCTime` now
                    runDB $ deleteWhere [DB.UserOtpCreated P.<=. cutoff]
                    checkOTP <- runDB $ selectList [ DB.UserOtpUser P.==. userId
                                                   , DB.UserOtpPassword P.==. otp
                                                   ] []
                    case checkOTP of
                     (_:_) -> do
                         runDB $ deleteWhere [ DB.UserOtpUser P.==. userId
                                             , DB.UserOtpPassword P.==. otp
                                             ]
                         Right <$> createToken userId
                     [] -> return $ Left LoginErrorFailed
           False -> return $ Left LoginErrorFailed
  where
    rehash userId = do
        mbNewHash <- hashPassword pwd
        case mbNewHash of
         Nothing -> return () -- TODO: log error
         Just newHash -> runDB $ P.update (DB.UserKey userId)
                                          [DB.UserPasswordHash P.=. newHash]
    createToken userId = do
        now <- liftIO $ getCurrentTime
        -- token <- liftIO $ b64Token <$> getEntropy 16 -- 128 bits
        token <- B64Token <$> mkRandomString tokenChars 22 -- > 128 bit
        _ <- runDB . P.insert $ DB.Token { DB.tokenToken = token
                                         , DB.tokenUser = userId
                                         , DB.tokenCreated = now
                                         , DB.tokenExpires = Nothing
                                         }
        return token
    b64Token = B64Token . Text.decodeUtf8 . B64.encode
    checkPassword (PasswordHash hash) (Password pwd') =
        BCrypt.validatePassword hash (Text.encodeUtf8 pwd')
    hashUsesPolicy (PasswordHash hash) =
        BCrypt.hashUsesPolicy policy hash
    createOTP p userId = do
        otp <- mkRandomOTP
        now <- liftIO getCurrentTime
        _ <- runDB $ insert DB.UserOtp { DB.userOtpUser = userId
                                       , DB.userOtpPassword = otp
                                       , DB.userOtpCreated = now
                                       }
        sendOTP username p otp
        return ()

checkToken :: B64Token -> API (Maybe UserID)
checkToken tokenId = do
    now <- liftIO $ getCurrentTime
    runDB $ deleteWhere [DB.TokenExpires P.<=. Just now]
    mbToken <- runDB $ P.get (DB.TokenKey tokenId)
    case mbToken of
     Nothing -> return Nothing
     Just token -> do
         return . Just $ DB.tokenUser token

logOut :: B64Token -> API ()
logOut token = do
    runDB $ delete (DB.TokenKey token)

-- addUser name password email mbPhone = do
