{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module SAML.Config where

import           Control.Monad.Logger    (logError)
import           Control.Monad.Trans
import qualified Data.ByteString         as BS
import qualified Data.Text               as Text
import qualified Data.UUID               as UUID
import           System.Exit             (exitFailure)

import           NejlaCommon.Config      hiding (Config)

import           SAML.Keys

import           Types

getSAMLConfig conf = do
  mbSamlPrivateKeyPath <- getConfMaybe "SAML_ENCRYPTION_PRIVATE_KEY_PATH"
    "saml.encryptionKeyPath" conf

  mbSamlCertPath <- getConfMaybe "SAML_SIGNING_CERTIFICATE_PATH"
    "saml.certPath" conf

  mbSamlAudience <- getConfMaybe "SAML_AUDIENCE" "saml.audience" conf
  mbSamlConfigDefaultInstanceTxt <- getConfMaybe "DEFAULT_INSTANCE"
                                                 "default-instance" conf
  mbSamlConfigIPBaseUrl <- getConfMaybe "SAML_IP_BASE_URL"
                                                 "saml.ip_base_url" conf

  case (mbSamlPrivateKeyPath, mbSamlCertPath, mbSamlAudience
       , mbSamlConfigDefaultInstanceTxt, mbSamlConfigIPBaseUrl
       ) of
    (Nothing, Nothing, Nothing, _, Nothing) -> return Nothing
    (Just privateKeyPath, Just certPath, Just samlConfigAudience
      , Just samlConfigDefaultInstanceTxt, Just samlConfigIPBaseUrl
      ) -> do
      privKeyBs <- liftIO .  BS.readFile $ Text.unpack privateKeyPath
      samlConfigEncryptionKey <- case parsePrivateKeyPem privKeyBs of
        Left e -> do
          $logError $ "Could not parse SAML private key: " <> (Text.pack  e)
          liftIO exitFailure
        Right r -> return r
      certBs <- liftIO . BS.readFile $ Text.unpack certPath
      samlConfigSigningKey <- case parseCertificatePem certBs of
        Left e -> do
          $logError $ "Could not parse SAML certificate: " <> (Text.pack e)
          liftIO exitFailure
        Right r -> return r

      samlConfigDefaultInstance <-
        case UUID.fromText samlConfigDefaultInstanceTxt of
          Nothing -> do
            $logError $ "Could not parse UUID: "
              <> (Text.pack $ show samlConfigDefaultInstanceTxt)
            liftIO exitFailure
          Just r -> return $ InstanceID r
      return (Just SamlConfig{..})
    _ -> do
      $logError "SAML configuration error: Partial configuration"
      liftIO exitFailure
