-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Database.Persist.Postgresql
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment

import           Api

connectionString :: ConnectionString
connectionString = ""

main :: IO ()
main = do
    runStderrLoggingT . withPostgresqlPool connectionString 5 $ \pool -> do
        liftIO $ Warp.run 3000 (serveAPI pool)
