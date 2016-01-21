-- Copyright (c) 2015 Lambdatrade AB
-- All Rights Reserved

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Config where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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

--------------------------------------------------------------------------------
-- Config file -----------------------------------------------------------------
--------------------------------------------------------------------------------

getConfGenericMaybe :: (MonadLogger m, MonadIO m, Conf.Configured a) =>
                       (String -> Maybe a)
                    -> String
                    -> Conf.Name
                    -> Conf.Config
                    -> m (Maybe a)
getConfGenericMaybe fromString env confName conf = do
    mbConfVal <- liftIO $ Conf.lookup conf confName
    case mbConfVal of
     Just v -> return $ Just v
     Nothing -> do
       mbVal <- liftIO $ lookupEnv env
       case mbVal of
        Nothing -> return Nothing
        Just v -> case fromString v of
                   Nothing -> do
                       $logError . Text.pack $
                            "Error reading environment variable \""
                            ++ env ++ "\": could not parse value " ++ show v
                       liftIO Exit.exitFailure
                   Just val -> return $ Just val

getConfGeneric :: (MonadLogger m, MonadIO m, Conf.Configured a) =>
                  (String -> Maybe a)
               -> String
               -> Conf.Name
               -> Either Text a
               -> Conf.Config
               -> m a
getConfGeneric fromString env confName mbDefault conf = do
    mbC <- getConfGenericMaybe fromString env confName conf
    case mbC of
     Nothing -> case mbDefault of
                 Right d -> return d
                 Left e -> do
                     $logError $ "Configuration of `"  <> e <> "` is required. \n"
                                 <> " Set environment variable "
                                 <> Text.pack env <>
                                 " or configuration variable " <> confName <> "."
                     liftIO $ Exit.exitFailure
     Just v -> return v

safeRead :: Read a => String -> Maybe a
safeRead str = case reads str of
     ((v,_):_) -> Just v
     _ -> Nothing

getConf' :: (Conf.Configured a, MonadLogger m, MonadIO m, Read a) =>
           String -> Conf.Name -> Either Text a -> Conf.Config -> m a
getConf' = getConfGeneric safeRead

getConfMaybe :: (MonadIO m, MonadLogger m) =>
                String
             -> Conf.Name
             -> Conf.Config
             -> m (Maybe Text)
getConfMaybe = getConfGenericMaybe (Just . Text.pack)

getConfMaybe' :: (Read a, MonadIO m, MonadLogger m, Conf.Configured a) =>
                 String
              -> Conf.Name
              -> Conf.Config
              -> m (Maybe a)
getConfMaybe' = getConfGenericMaybe safeRead

getConf :: (MonadLogger m, MonadIO m) =>
            String -> Conf.Name -> Either Text Text -> Conf.Config -> m Text
getConf = getConfGeneric (Just . Text.pack)

loadConf :: MonadIO m => m Conf.Config
loadConf = liftIO $ do
    mbConfPath <- lookupEnv "AUTH_SERVICE_CONF_PATH"
    let confFiles =  catMaybes [ Just $ Conf.Optional "/data/auth_service.conf"
                               , Conf.Required <$> mbConfPath
                               ]
    debug $ "Loading conf files " <> showText confFiles
    Conf.load confFiles

--------------------------------------------------------------------------------
-- Configuration ---------------------------------------------------------------
--------------------------------------------------------------------------------

getDBString :: (MonadLogger m, MonadIO m) => Conf.Config -> m ByteString
getDBString conf = do
    host <- getConf "AUTH_SERVICE_DB_HOST" "db.host" (Right "") conf
    usr <- getConf "AUTH_SERVICE_DB_USER" "db.user" (Right "") conf
    db   <- getConf "AUTH_SERVICE_DB_DATABASE" "db.database" (Right "auth-service")
                    conf
    pwd <- getConf "AUTH_SERVICE_DB_PASSWORD" "db.password"
                        (Right "") conf
    return . BS.intercalate " "
        $ [ "host"     .= host
          , "user"     .= usr
          , "dbname"   .= db
          , "password" .= pwd
          ]
  where
    _ .= "" = ""
    k .= v = k <> "=" <> (Text.encodeUtf8 v)

getTwilioConfig :: (MonadIO m, MonadLogger m) =>
                   Conf.Config
                -> m (Maybe TwilioConfig)
getTwilioConfig conf = do
    mbAccount <- getConfMaybe "AUTH_SERVICE_TWILIO_ACCOUNT" "twilio.account" conf
    mbAuthToken <- getConfMaybe "AUTH_SERVICE_TWILIO_TOKEN" "twilio.token" conf
    mbSourceNumber <- getConfMaybe "AUTH_SERICE_TWILIO_SOURCE" "twilio.source" conf
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


getAuthServiceConfig :: (MonadIO m, MonadLogger m) =>
                        Conf.Config
                     -> m Config
getAuthServiceConfig conf = do
    timeout <- getConf' "AUTH_SERVICE_TOKEN_TIMEOUT" "token.timeout"
                 (Right 3600) {- 1 hour -} conf
    dbString <- getDBString conf
    otpl <- getConf' "AUTH_SERVICE_OTP_LENGTH" "otp.length"
                 (Right 8) conf
    otpt <- getConf' "AUTH_SERVICE_OTP_TIMEOUT" "otp.timeout"
                 (Right 300) conf
    twilioConf <- getTwilioConfig conf
    return Config{ configTimeout = timeout
                 , configDbString = dbString
                 , configOTPLength = otpl
                 , configOTPTimeoutSeconds = otpt
                 , configTwilio = twilioConf
                 }
