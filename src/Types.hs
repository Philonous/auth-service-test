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
import Web.PathPieces

import Helpers

newtype Username = Username{ unUsername :: Text}
                   deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece
                            , PersistField, PersistFieldSql, ToJSON, FromJSON
                            , IsString, ToByteString
                            )

newtype Password = Password{ unPassword :: Text}
                   deriving ( Show, Read, Eq, Ord, Typeable, Data
                            , PersistField, PersistFieldSql, ToJSON, FromJSON
                            , IsString
                            )

newtype PasswordHash = PasswordHash{ unPasswordHash :: ByteString}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , PersistField, PersistFieldSql
                            )

newtype Email = Email{ unEmail :: Text}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , PersistField, PersistFieldSql
                            , ToJSON, FromJSON
                            , IsString
                            )

newtype Phone = Phone { unPhone :: Text}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , PersistField, PersistFieldSql, ToJSON, FromJSON
                            , IsString
                            )

newtype B64Token = B64Token { unB64Token :: Text }
                   deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece
                            , PersistField, PersistFieldSql
                            , FromText
                            )

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "unB64"} ''B64Token

data CreateUser = CreateUser { createUserName     :: !Username
                             , createUserPassword :: !Password
                             , createUserEmail    :: !Email
                             , createUserPhone    :: !(Maybe Phone)
                             } deriving (Show)

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "createUser"} ''CreateUser
makeLensesWith camelCaseFields ''CreateUser

data Login = Login { loginUser     :: !Username
                   , loginPassword :: !Password
                   } deriving ( Show, Read, Eq, Ord, Typeable, Data )



deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "login"} ''Login
makeLensesWith camelCaseFields ''Login

--------------------------------------------------------------------------------
-- Monad -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data ApiState = ApiState { apiStateSqlCon :: SqlBackend }

makeLensesWith camelCaseFields ''ApiState

newtype API a = API { unAPI :: ReaderT ApiState IO a }
              deriving ( Functor, Applicative, Monad, MonadIO
                       , MonadThrow, MonadCatch)

runDB :: ReaderT SqlBackend IO a -> API a
runDB m = do
    con <- API $ view sqlCon
    liftIO $ runReaderT m con

runAPI :: ConnectionPool -> API a -> IO a
runAPI pool (API m) = flip runSqlPool pool . ReaderT $ \con ->
    let state = ApiState { apiStateSqlCon = con }
    in runReaderT m state
