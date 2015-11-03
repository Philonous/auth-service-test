-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Text.Strict.Lens
import           Database.Persist.Postgresql
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment
import           System.Exit

import           Api
import           Config
import           Persist.Schema
import           Types
import           User

main :: IO ()
main = runStderrLoggingT $ do
    confFile <- loadConf
    conf <- getAuthServiceConfig confFile
    let connectionString = conf ^. dbString
    liftIO . putStrLn $ "Connecting to DB " ++ show connectionString
    withPostgresqlPool connectionString 5 $ \pool -> do
        let run = liftIO . runAPI pool conf
        liftIO $ runSqlPool (runMigration migrateAll) pool
        args <- liftIO getArgs
        case args of
         ("adduser": args') -> do
             res <- run $ addUser (args' ^.. each . packed)
             case res of
              Nothing -> liftIO $ do
                  putStrLn "Could not add user"
                  exitFailure
              Just () -> return ()
         ["run"] -> liftIO $ Warp.run 3000 (serveAPI pool conf)
         _ -> liftIO $ do
             putStrLn "Usage: auth-service [adduser|run] [options]"
             exitFailure
