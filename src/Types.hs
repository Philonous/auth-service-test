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

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Data
import Data.String
import Data.Text (Text)
import Database.Persist.Sql
import Servant
import Web.HttpApiData
import Web.PathPieces

import Helpers

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

data ReturnUser = ReturnUser { returnUserUser :: !Username }

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

data Config = Config { configTimeout :: !Integer -- token timeout in seconds
                     , configDbString :: !ByteString
                     , configOTPLength :: !Int
                     , configOTPTimeoutSeconds :: !Integer
                     }

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
