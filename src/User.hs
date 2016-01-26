-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE OverloadedStrings #-}

module User where

import           Control.Applicative
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.IO

import           Backend
import qualified Persist.Schema as DB
import           System.Exit
import           Types

addUser :: [Text] -> API (Maybe ())
addUser args = do
    case args of
     (username : pwd : emailAddr : mbPhone) -> do
         createUser AddUser{ addUserName     = Username username
                           , addUserPassword = Password pwd
                           , addUserEmail    = Email emailAddr
                           , addUserPhone    = Phone <$> listToMaybe mbPhone
                           }
     _ -> liftIO $ do
         hPutStrLn stderr
             "usage: auth-service adduser <username> <password> <email> [<phone>]"
         exitFailure

changePassword :: [String] -> API ()
changePassword args = do
  case Text.pack <$> args of
   [username, pwd] -> do
     mbUser <- getUserByName (Username username)
     case mbUser of
      Nothing -> liftIO $ hPutStrLn stderr "User not found"
      Just user -> do
          res <- changeUserPassword (DB.userUuid user) (Password pwd)
          case res of
           Nothing -> liftIO $ do
               hPutStrLn stderr "chpass: Could not create password hash"
               exitFailure
           _ -> return ()
   _ -> liftIO $ do
         hPutStrLn stderr
             "usage: auth-service chpass <username> <password>"
         exitFailure
