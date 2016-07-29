-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

-- | Import this module qualified like this:
--
-- >>> import qualified Logging as Log

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Logging where

import           Control.Monad.Trans
import qualified Data.Aeson as Aeson
import           Data.Aeson.TH
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.IO as Text
import           Data.Time.Clock
import           Database.Persist
import           NejlaCommon
import           System.IO
import           Types

import qualified Persist.Schema as DB

data LogLevel = Debug | Info | Warn | Error deriving (Eq, Show)

logLevelToText :: LogLevel -> Text
logLevelToText Debug  = "DEBUG"
logLevelToText Info  = "INFO"
logLevelToText Warn  = "WARN"
logLevelToText Error = "ERROR"

logMessage :: LogLevel -> Text -> API ()
logMessage level msg = do
    liftIO . Text.hPutStrLn stderr $ Text.concat [ "[", logLevelToText level , "] "
                                                 ,  msg
                                                 ]

logDebug, logInfo, logWarn, logError :: Text -> API ()
logDebug =  logMessage Debug
logInfo = logMessage Info
logWarn = logMessage Warn
logError = logMessage Error

logES :: LogMessage a =>
         a
      -> API ()
logES msg = do
  now <- liftIO getCurrentTime
  let pl = case Aeson.toJSON msg of
                  Aeson.Object o -> Aeson.Object (HMap.delete "type" o)
                  v -> v
  _ <- runDB $ insert DB.Log { DB.logTime = now
                             , DB.logType = messageType msg
                             , DB.logPayload = pl
                             }
  return ()

type Request = Text

data LogEvent
  = OTPSent{ user :: !Email, otp:: !Password}
  | AuthSuccess{ user:: !Email, token :: !Text}
  | AuthSuccessOTP{ user:: !Email, otp:: !Password, token :: !Text}
  | AuthFailed{ user:: !Email, triedOtp:: !(Maybe Password)}
  | Request{ user:: !Email, request :: !Request, token :: !Text}
  | RequestNoToken{request:: !Request, instanceId :: !InstanceID}
  | RequestInvalidToken{ request :: !Request
                               , token:: !Text
                               , instanceId :: !InstanceID
                               }
  | RequestInvalidInstance{ user :: !Email
                          , request :: !Request
                          , token :: !Text
                          , instanceId :: !InstanceID
                          }
  | Logout{user ::  !Email}
  | UserCreated {user :: !Email }
  | PasswordChangeFailed {user :: !Email }
  | PasswordChanged {user :: !Email }
  deriving (Show)

deriveJSON defaultOptions{sumEncoding = TaggedObject "type" "contents"}
           ''LogEvent

instance LogMessage LogEvent where
  messageType OTPSent{}                = "otp_sent"
  messageType AuthSuccess{}            = "auth_success"
  messageType AuthSuccessOTP{}         = "auth_success"
  messageType AuthFailed{}             = "auth_failed"
  messageType Request{}                = "request"
  messageType RequestNoToken{}         = "request_no_token"
  messageType RequestInvalidToken{}    = "request_invalid_token"
  messageType RequestInvalidInstance{} = "request_invalid_instance"
  messageType Logout{}                 = "logout"
  messageType UserCreated{}            = "user_created"
  messageType PasswordChangeFailed{}   = "password_change_failed"
  messageType PasswordChanged{}        = "password_changed"
