-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE FlexibleContexts #-}
module Backend
  (module Backend
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock
import qualified Database.Persist as P
import           Database.Persist.Sql
import           System.Entropy

import qualified Persist.Schema as DB
import           Types


policy :: BCrypt.HashingPolicy
policy = BCrypt.fastBcryptHashingPolicy { BCrypt.preferredHashCost = 10 }

tokenTimeoutSeconds :: Integer
tokenTimeoutSeconds = 300 -- 5 minutes

hashPassword :: Password -> API (Maybe PasswordHash)
hashPassword (Password pwd) = liftIO $
    fmap PasswordHash <$>
      BCrypt.hashPasswordUsingPolicy policy (Text.encodeUtf8 pwd)

createUser :: AddUser -> API (Maybe ())
createUser usr = do
    mbHash <- hashPassword $ usr ^. password
    case mbHash of
     Nothing -> return Nothing
     Just hash -> do
       let dbUser = DB.User { DB.userName = usr ^. name
                            , DB.userPasswordHash = hash
                            , DB.userEmail = usr ^. email
                            , DB.userPhone = usr ^. phone
                            }
       _ <- runDB $ P.insert dbUser
       return $ Just ()

login :: Login -> API (Maybe B64Token)
login Login{ loginUser = username
           , loginPassword = pwd
           } = do
    mbUser <- runDB $ P.get (DB.UserKey username)
    case mbUser of
     Nothing -> return Nothing
     Just usr -> do
         let hash = usr ^. DB.passwordHash
         case checkPassword hash pwd of
           True -> do
               unless (hashUsesPolicy hash) rehash
               Just <$> createToken
           False -> return Nothing
  where
    rehash = do
        mbNewHash <- hashPassword pwd
        case mbNewHash of
         Nothing -> return () -- TODO: log error
         Just newHash -> runDB $ P.update (DB.UserKey username)
                                          [DB.UserPasswordHash P.=. newHash]
    createToken = do
        now <- liftIO $ getCurrentTime
        token <- liftIO $ b64Token <$> getEntropy 16 -- 128 bits
        _ <- runDB . P.insert $ DB.Token { DB.tokenToken = token
                                         , DB.tokenUser = username
                                         , DB.tokenCreated = now
                                         , DB.tokenExpires = Nothing
                                         }
        return token
    b64Token = B64Token . Text.decodeUtf8 . B64.encode
    checkPassword (PasswordHash hash) (Password pwd') =
        BCrypt.validatePassword hash (Text.encodeUtf8 pwd')
    hashUsesPolicy (PasswordHash hash) =
        BCrypt.hashUsesPolicy policy hash


checkToken :: B64Token -> API (Maybe Username)
checkToken tokenId = do
    now <- liftIO $ getCurrentTime
    runDB $ deleteWhere [DB.TokenExpires P.<=. Just now]
    mbToken <- runDB $ P.get (DB.TokenKey tokenId)
    case mbToken of
     Nothing -> return Nothing
     Just token -> return . Just $ DB.tokenUser token

-- addUser name password email mbPhone = do
