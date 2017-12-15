-- Copyright (c) 2015 Lambdatrade AB
-- All Rights Reserved

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}

module Config
  ( module NejlaCommon.Config
  , module Config
  ) where

import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.ByteString            as BS
import qualified Data.Configurator.Types    as Conf
import           Data.Default               (def)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.Mail.Mime          as Mail
import qualified System.Exit                as Exit
import qualified Text.Microstache           as Mustache
import           Text.StringTemplate
import           Text.StringTemplate.QQ

import           Types
import           Util

import           NejlaCommon.Config         hiding (Config)

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
        (Just acc, Just authT, Just sourceNo) ->
            return $ Just TwilioConfig { twilioConfigAccount = acc
                                       , twilioConfigAuthToken = authT
                                       , twilioConfigSourceNumber = sourceNo
                                       }
        _ -> do
            $logError "Twilio config is incomplete"
            liftIO Exit.exitFailure

get2FAConf :: (MonadLogger m, MonadIO m) =>
              Conf.Config
           -> m (Bool, Maybe TwilioConfig)
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
    to <- getConf' "TOKEN_TIMEOUT" "token.timeout"
                 (Right 3600) {- 1 hour -} conf
    otpl <- getConf' "OTP_LENGTH" "otp.length"
                 (Right 4) conf
    otpt <- getConf' "OTP_TIMEOUT" "otp.timeout"
                 (Right 300) conf
    (tfaRequired, twilioConf) <- get2FAConf conf
    haveEmail <- setEmailConf conf
    return Config{ configTimeout = to
                 , configOTPLength = otpl
                 , configOTPTimeoutSeconds = otpt
                 , configTFARequired = tfaRequired
                 , configTwilio = twilioConf
                 , configUseTransactionLevels = True
                 , configEmail = haveEmail
                 }

-- Default template loaded from src/password-reset-email-template.html.mustache
defaultTemplate :: Mustache.Template
defaultTemplate =
  $(do
       let templatePath = "src/html/password-reset-email-template.html.mustache"
       TH.addDependentFile templatePath
       template <- TH.runIO $ Mustache.compileMustacheFile templatePath
       TH.lift template
   )

setEmailConf :: (MonadIO m, MonadLogger m) => Conf.Config -> m (Maybe EmailConfig)
setEmailConf conf =
  getConfMaybe "EMAIL_FROM" "email.from" conf >>= \case
    Nothing -> return Nothing
    Just fromAddress -> do
      fromName <- getConfMaybe "EMAIL_FROM_NAME" "email.fromName" conf
      let emailConfigFrom = Mail.Address fromName fromAddress
      emailConfigHost <-
        getConf "EMAIL_SMTP" "email.smtp" (Left "email host") conf
      emailConfigUser <-
        getConf "EMAIL_USER" "email.user" (Left "email user") conf
      emailConfigPassword <-
        getConf "EMAIL_PASSWORD" "email.password" (Left "email password") conf
      emailConfigSiteName <-
        getConf "SITE_NAME" "site-name" (Left "site name") conf
      emailConfigResetLinkExpirationTime <-
        getConf "RESET_LINK_EXPIRATION_TIME" "email.link-expiration-time"
                 (Left "Human-readable reset link expiration time") conf
      mbEmailConfigTemplatefile <-
        getConfMaybe
          "EMAIL_TEMPLATE"
          "email.template"
          conf
      emailConfigTemplate <-
        case mbEmailConfigTemplatefile of
          Nothing -> return defaultTemplate
          Just filename -> liftIO . Mustache.compileMustacheFile $
                             Text.unpack filename
      let
        emailConfigSendmail = def
        ecfg = EmailConfig {..}
      liftIO $ writeMsmtprc ecfg
      return $ Just ecfg

writeMsmtprc ::  EmailConfig -> IO ()
writeMsmtprc emailConfig =
  BS.writeFile "/etc/msmtprc" bs
  where bs = Text.encodeUtf8 $ renderMsmtprc emailConfig


renderMsmtprc :: EmailConfig -> Text
renderMsmtprc EmailConfig{..} =
  let Mail.Address _ fromAddress = emailConfigFrom
  in render $ [stmp|
defaults

tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt

timeout 30

account defaultaccount
host $`emailConfigHost`$
port 587
from $`fromAddress`$
auth on
user $`emailConfigUser`$
password $`emailConfigPassword`$

account default : defaultaccount
|]
