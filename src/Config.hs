{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- Copyright (c) 2015 Lambdatrade AB
-- All Rights Reserved

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
       return $ fromString =<< mbVal

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

getConf :: (MonadLogger m, MonadIO m) =>
            String -> Conf.Name -> Either Text Text -> Conf.Config -> m Text
getConf = getConfGeneric (Just . Text.pack)

loadConf :: MonadIO m => m Conf.Config
loadConf = liftIO $ do
    mbConfPath <- lookupEnv "AUTH_SERVICE_CONF_PATH"
    Conf.load $ catMaybes [ Just $ Conf.Optional "/data/auth_service.conf"
                          , Conf.Required <$> mbConfPath
                          ]

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
    k .= "" = ""
    k .= v = k <> "=" <> (Text.encodeUtf8 v)

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
    return Config{ configTimeout = timeout
                 , configDbString = dbString
                 , configOTPLength = otpl
                 , configOTPTimeoutSeconds = otpt
                 }
