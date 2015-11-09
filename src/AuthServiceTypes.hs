-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module AuthServiceTypes where

import           Control.Lens
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
import           Web.HttpApiData
import           Web.PathPieces

import           Helpers

newtype UserID = UserID { unUserID :: UUID.UUID }
                 deriving ( Show, Read, Eq, Ord, Typeable, Data
                          )

makePrisms ''UserID

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
                            , ToJSON, FromJSON
                            , IsString, ToByteString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Username

newtype Password = Password{ unPassword :: Text}
                   deriving ( Show, Read, Eq, Ord, Typeable, Data
                            , ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Password

newtype PasswordHash = PasswordHash{ unPasswordHash :: ByteString}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            )

makePrisms ''PasswordHash

newtype Email = Email{ unEmail :: Text}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Email

newtype Phone = Phone { unPhone :: Text}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Phone

newtype B64Token = B64Token { unB64Token :: Text }
                   deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece
                            , ToByteString
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
                    deriving (Show, Eq)

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "returnUser"}
    ''ReturnUser
makeLensesWith camelCaseFields ''ReturnUser

data Login = Login { loginUser     :: !Username
                   , loginPassword :: !Password
                   , loginOtp      :: !(Maybe Password)
                   } deriving ( Show, Read, Eq, Ord, Typeable, Data )



deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "login"} ''Login
makeLensesWith camelCaseFields ''Login
