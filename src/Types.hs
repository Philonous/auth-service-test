{-# LANGUAGE RankNTypes #-}
-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString (ByteString)
import           Data.ByteString.Conversion
import           Data.Data
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import           Database.Persist.Sql
import           Servant
import           Twilio.Types as Twilio
import           Web.HttpApiData
import           Web.PathPieces

import           Helpers

newtype UserID = UserID { unUserID :: UUID.UUID }
                 deriving ( Show, Read, Eq, Ord, Typeable, Data
                          )

makePrisms ''UserID

instance PersistField UserID where
    toPersistValue = toPersistValue . UUID.toString . unUserID
    fromPersistValue = \x -> case x of
        PersistDbSpecific bs ->
            case UUID.fromASCIIBytes bs of
             Nothing -> Left $ "Invalid UUID: " <> (Text.pack $ show bs)
             Just u -> Right $ UserID u
        PersistText txt ->
            case UUID.fromString $ Text.unpack txt of
             Nothing -> Left $ "Invalid UUID: " <> (Text.pack $ show txt)
             Just u -> Right $ UserID u
        e -> Left $ "Can not convert to uuid: " <> (Text.pack $ show e)

instance PersistFieldSql UserID where
    sqlType _ = SqlOther "uuid"

instance PathPiece UserID where
    fromPathPiece = fmap UserID . UUID.fromText
    toPathPiece = Text.pack . UUID.toString . unUserID

instance ToHttpApiData UserID where
    toUrlPiece = toPathPiece

instance FromHttpApiData UserID where
    parseUrlPiece txt =
        case fromPathPiece txt of
         Nothing -> Left $ "Could not parse user id " <> txt
         Just uuid -> Right uuid

instance ToJSON UserID where
    toJSON (UserID uid) = toJSON $ UUID.toText uid

instance FromJSON UserID where
    parseJSON v = do
        txt <- parseJSON v
        case UUID.fromText txt of
         Nothing -> fail $ "Can't parse UUID " <> (Text.unpack txt)
         Just uuid -> return $ UserID uuid

instance ToByteString UserID where
    builder = builder . Text.encodeUtf8 . UUID.toText . unUserID

newtype Username = Username{ unUsername :: Text}
                   deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece
                            , PersistField, PersistFieldSql, ToJSON, FromJSON
                            , IsString, ToByteString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Username

newtype Password = Password{ unPassword :: Text}
                   deriving ( Show, Read, Eq, Ord, Typeable, Data
                            , PersistField, PersistFieldSql, ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Password

newtype PasswordHash = PasswordHash{ unPasswordHash :: ByteString}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , PersistField, PersistFieldSql
                            )

makePrisms ''PasswordHash

newtype Email = Email{ unEmail :: Text}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , PersistField, PersistFieldSql
                            , ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Email

newtype Phone = Phone { unPhone :: Text}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , PersistField, PersistFieldSql, ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Phone

newtype B64Token = B64Token { unB64Token :: Text }
                   deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece
                            , PersistField, PersistFieldSql
                            , FromText, ToByteString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''B64Token

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "unB64"} ''B64Token

data AddUser = AddUser { addUserName     :: !Username
                       , addUserPassword :: !Password
                       , addUserEmail    :: !Email
                       , addUserPhone    :: !(Maybe Phone)
                       } deriving (Show)

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "addUser"} ''AddUser
makeLensesWith camelCaseFields ''AddUser

data ReturnUser = ReturnUser { returnUserUser :: !UserID }

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "returnUser"}
    ''ReturnUser
makeLensesWith camelCaseFields ''ReturnUser

data Login = Login { loginUser     :: !Username
                   , loginPassword :: !Password
                   , loginOtp      :: !(Maybe Password)
                   } deriving ( Show, Read, Eq, Ord, Typeable, Data )



deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "login"} ''Login
makeLensesWith camelCaseFields ''Login

--------------------------------------------------------------------------------
-- Error -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data LoginError = LoginErrorFailed -- Username not found, password wrong or OTP
                                   -- wrong
                | LoginErrorOTPRequired
                  deriving (Show, Eq)

makePrisms ''LoginError

--------------------------------------------------------------------------------
-- Config ----------------------------------------------------------------------
--------------------------------------------------------------------------------


data TwilioConfig = TwilioConfig { twilioConfigAccount :: !Twilio.AccountSID
                                 , twilioConfigAuthToken :: !Twilio.AuthToken
                                 , twilioConfigSourceNumber :: !Text
                                 } deriving Show

makeLensesWith camelCaseFields ''TwilioConfig

data Config = Config { configTimeout :: !Integer -- token timeout in seconds
                     , configDbString :: !ByteString
                     , configOTPLength :: !Int
                     , configOTPTimeoutSeconds :: !Integer
                     , configTwilio :: !TwilioConfig
                     } deriving Show

makeLensesWith camelCaseFields ''Config

--------------------------------------------------------------------------------
-- Monad -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data ApiState = ApiState { apiStateSqlCon :: SqlBackend
                         , apiStateConfig :: Config
                         }

makeLensesWith camelCaseFields ''ApiState

newtype API a = API { unAPI :: ReaderT ApiState IO a }
              deriving ( Functor, Applicative, Monad, MonadIO
                       , MonadThrow, MonadCatch)

runDB :: ReaderT SqlBackend IO a -> API a
runDB m = do
    con <- API $ view sqlCon
    liftIO $ runReaderT m con

getConfig ::  Lens' Config a -> API a
getConfig g = API . view $ config . g

runAPI :: ConnectionPool -> Config -> API a -> IO a
runAPI pool conf (API m) = flip runSqlPool pool . ReaderT $ \con ->
    let state = ApiState { apiStateSqlCon = con
                         , apiStateConfig = conf
                         }
    in runReaderT m state
