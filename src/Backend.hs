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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock
import qualified Database.Persist as P
import           Database.Persist.Sql
import           System.Entropy
import           System.Random

import qualified Persist.Schema as DB
import           Types


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
    mbHash <- hashPassword $ usr ^. password
    case mbHash of
     Nothing -> return Nothing
     Just hash -> do
       let dbUser = DB.User { DB.userName = usr ^. name
                            , DB.userPasswordHash = hash
                            , DB.userEmail = usr ^. email
                            , DB.userPhone = usr ^. phone
                            }
       _ <- runDB $ P.insert dbUser
       return $ Just ()

otpIDChars :: [Char]
otpIDChars = "CDFGHJKLMNPQRSTVWXYZ2345679"

mkRandomOTP ::  API Password
mkRandomOTP = do
    len <- getConfig oTPLength
    liftIO $ Password . Text.pack <$> replicateM len (selectOne otpIDChars)
  where
    selectOne xs = do
        i <- randomRIO (0, length xs - 1)
        return $ xs !! i

sendOTP :: Phone -> Password -> API ()
sendOTP p otp = liftIO . putStrLn $ "Sending OTP: " <> show p <> " " <> show otp

login :: Login -> API (Either LoginError B64Token)
login Login{ loginUser = username
           , loginPassword = pwd
           , loginOTP      = mbOtp
           } = do
    mbUser <- runDB $ P.get (DB.UserKey username)
    case mbUser of
     Nothing -> return $ Left LoginErrorFailed
     Just usr -> do
         let hash = usr ^. DB.passwordHash
         case checkPassword hash pwd of
           True -> do
               unless (hashUsesPolicy hash) rehash
               case mbOtp of
                Nothing -> case usr ^. phone of
                            -- No phone number for two-factor auth
                            Nothing -> Right <$> createToken
                            Just p  -> do
                                createOTP p
                                return $ Left LoginErrorOTPRequired
                Just otp -> do
                    otpTime <- fromIntegral . negate <$> getConfig oTPTimeoutSeconds
                    now <- liftIO getCurrentTime
                    let cutoff = otpTime `addUTCTime` now
                    runDB $ deleteWhere [DB.UserOtpCreated P.<=. cutoff]
                    checkOTP <- runDB $ selectList [ DB.UserOtpUser P.==. username
                                                   , DB.UserOtpPassword P.==. otp
                                                   ] []
                    case checkOTP of
                     (_:_) -> do
                         runDB $ deleteWhere [ DB.UserOtpUser P.==. username
                                             , DB.UserOtpPassword P.==. otp
                                             ]
                         Right <$> createToken
                     [] -> return $ Left LoginErrorFailed
           False -> return $ Left LoginErrorFailed
  where
    rehash = do
        mbNewHash <- hashPassword pwd
        case mbNewHash of
         Nothing -> return () -- TODO: log error
         Just newHash -> runDB $ P.update (DB.UserKey username)
                                          [DB.UserPasswordHash P.=. newHash]
    createToken = do
        now <- liftIO $ getCurrentTime
        token <- liftIO $ b64Token <$> getEntropy 16 -- 128 bits
        _ <- runDB . P.insert $ DB.Token { DB.tokenToken = token
                                         , DB.tokenUser = username
                                         , DB.tokenCreated = now
                                         , DB.tokenExpires = Nothing
                                         }
        return token
    b64Token = B64Token . Text.decodeUtf8 . B64.encode
    checkPassword (PasswordHash hash) (Password pwd') =
        BCrypt.validatePassword hash (Text.encodeUtf8 pwd')
    hashUsesPolicy (PasswordHash hash) =
        BCrypt.hashUsesPolicy policy hash
    createOTP p = do
        otp <- mkRandomOTP
        now <- liftIO getCurrentTime
        _ <- runDB $ insert DB.UserOtp { DB.userOtpUser = username
                                       , DB.userOtpPassword = otp
                                       , DB.userOtpCreated = now
                                       }
        sendOTP p otp
        return ()

checkToken :: B64Token -> API (Maybe Username)
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
