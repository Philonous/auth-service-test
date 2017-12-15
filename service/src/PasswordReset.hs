{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module PasswordReset where

import qualified Control.Exception    as Ex (ErrorCall(..))
import           Control.Lens
import qualified Control.Monad.Catch  as Ex
import qualified Control.Monad.Logger as Log
import           Control.Monad.Trans
import qualified Data.Aeson           as Aeson
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Lazy       as LText
import           NejlaCommon          (viewState)
import qualified Network.Mail.Mime    as Mail
import qualified Text.Microstache     as Mustache

import           Backend
import           Logging
import           Types

-- | Create an Aeson object from password reset email data
fromEmailData :: EmailData -> Aeson.Value
fromEmailData emailData =
  Aeson.object [ "address" .= (emailData ^. address)
               , "link" .= (emailData ^. link)
               , "expirationTime" .= emailData ^. expirationTime
               , "siteName" .= emailData ^. siteName
               ]
  where
    infix 0 .=
    (.=) = (Aeson..=)


renderEmail ::
     (Ex.MonadThrow m, Log.MonadLogger m)
  => EmailConfig
  -> EmailData
  -> m LText.Text
renderEmail conf emailData =
  let (warnings, result) =
        Mustache.renderMustacheW (conf ^. template) $ fromEmailData emailData
  in case warnings of
       [] -> return result
       (_:_) -> do
         logError $
           "Could not render email, Microstache warnings: " <>
           Text.pack (show warnings)
         Ex.throwM  EmailRenderError
  where
    (.=) = (Aeson..=)

sendPasswordResetEmail ::
     (Ex.MonadThrow m, Log.MonadLogger m, MonadIO m) =>
     EmailConfig
  -> Text -- ^ Email address
  -> m Bool
sendPasswordResetEmail cfg toAddress = do
  let sendmailCfg = cfg ^. sendmail
  logES $ PasswordResetRequested (Email toAddress)
  body <-
    renderEmail
      cfg
      EmailData
      { emailDataSiteName = cfg ^. siteName
      , emailDataExpirationTime =  cfg ^.resetLinkExpirationTime
      , emailDataAddress = toAddress
      , emailDataLink = "" -- @TODO
      }
  let subject = "Password reset"
      plainBody = "Please see the HTML attachment."
      to = Mail.Address Nothing toAddress
  let mail =
        Mail.addPart [Mail.plainPart plainBody, Mail.htmlPart body] $
        (Mail.emptyMail to) {Mail.mailHeaders = [("Subject", subject)]}
  mbError <-
    liftIO . Ex.try $
    Mail.renderSendMailCustom
      (sendmailCfg ^. path)
      (sendmailCfg ^. arguments)
      mail
  case mbError of
    Left (Ex.ErrorCall msg) -> do
      logError $ "Error sending password reset mail: " <> Text.pack msg
      return False
    Left e -> Ex.throwM e
    Right () -> return True

-- passwordResetRequest user = do
--   emailCfg <- viewState $ config . email >>= \case
--     Nothing -> Ex.throwM ChangeP
--   mbUser <- getUserByEmail user
--   case mbUser of
--     Just user -> sendPasswordResetEmail emailCfg (user ^. email)
--     Nothing -> Ex.throwM ChangePasswordUserDoesNotExistError
