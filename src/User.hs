-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE OverloadedStrings #-}

module User where

import           Control.Applicative
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Text (Text)

import           System.Exit
import           Types
import           Backend

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
         putStrLn "usage: auth-service adduser <username> <password> <email> [<phone>]"
         exitFailure
