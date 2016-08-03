-- Copyright (c) 2015 Lambdatrade AB
-- All Rights Reserved

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Config
  ( module NejlaCommon.Config
  , module Config
  ) where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.Environment
import qualified System.Exit as Exit

import           Helpers
import           Types

import           NejlaCommon.Config hiding (Config)
import qualified NejlaCommon.Config as Conf
--------------------------------------------------------------------------------
-- Configuration ---------------------------------------------------------------
--------------------------------------------------------------------------------

getTwilioConfig :: (MonadIO m, MonadLogger m) =>
                   Conf.Config
                -> m (Maybe TwilioConfig)
getTwilioConfig conf = do
    mbAccount <- getConfMaybe "TWILIO_ACCOUNT" "twilio.account" conf
    mbAuthToken <- getConfMaybe "TWILIO_TOKEN" "twilio.token" conf
    mbSourceNumber <- getConfMaybe "TWILIO_SOURCE" "twilio.source" conf
    case (mbAccount, mbAuthToken, mbSourceNumber) of
        (Nothing, Nothing, Nothing) -> return Nothing
        (Just account, Just authToken, Just sourceNumber) ->
            return $ Just TwilioConfig { twilioConfigAccount = account
                                       , twilioConfigAuthToken = authToken
                                       , twilioConfigSourceNumber = sourceNumber
                                       }
        _ -> do
            $logError "Twilio config is incomplete"
            liftIO Exit.exitFailure

get2FAConf conf = do
     tfaRequired <- getConfBool "TFA_REQUIRED" "tfa.required" (Right False) conf
     twilioConf <- getTwilioConfig conf
     case (tfaRequired, twilioConf) of
      (True, Nothing) -> do
          $logError "Two Factor Authentication is required, but Twilio is not configured"
          liftIO Exit.exitFailure
      _ -> return (tfaRequired, twilioConf)


getAuthServiceConfig :: (MonadIO m, MonadLogger m) =>
                        Conf.Config
                     -> m Config
getAuthServiceConfig conf = do
    timeout <- getConf' "TOKEN_TIMEOUT" "token.timeout"
                 (Right 3600) {- 1 hour -} conf
    otpl <- getConf' "OTP_LENGTH" "otp.length"
                 (Right 4) conf
    otpt <- getConf' "OTP_TIMEOUT" "otp.timeout"
                 (Right 300) conf
    (tfaRequired, twilioConf) <- get2FAConf conf
    return Config{ configTimeout = timeout
                 , configOTPLength = otpl
                 , configOTPTimeoutSeconds = otpt
                 , configTFARequired = tfaRequired
                 , configTwilio = twilioConf
                 }
