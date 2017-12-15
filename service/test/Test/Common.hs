{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Common where

import           Control.Monad.Logger
import           Data.Default
import           Data.Pool
import           Database.Persist.Sqlite as SQLite
import           Test.Hspec.Wai
import qualified Text.Microstache        as Mustache

import qualified Persist.Schema          as DB
import           Types

withMemoryPool :: (Pool SqlBackend -> IO a) -> IO a
withMemoryPool f = runNoLoggingT . withSqliteConn ":memory:" $ \con -> liftIO $ do
  pool <- createPool (return con) (\_ -> return ()) 1 3600 1
  f pool

testEmailConfig :: EmailConfig
testEmailConfig =
  EmailConfig
  { emailConfigHost     = "localhost"
  , emailConfigFrom     = "testuser@localhost"
  , emailConfigUser     = "testuser"
  , emailConfigPassword = "pwd"
  , emailConfigTemplate = tmpl
  , emailConfigSendmail =
      SendmailConfig
      { sendmailConfigPath = "/usr/bin/cat"
      , sendmailConfigArguments = []
      }
  , emailConfigSiteName = "Test Site"
  , emailConfigResetLinkExpirationTime = "24 hours"
  }
  where
    Right tmpl =
      Mustache.compileMustacheText
        "email template"
        "{{address}} please click on {{link}}"


withApiData :: (Pool SqlBackend -> Config -> IO a) -> IO a
withApiData f = withMemoryPool $ \pool -> do
  let conf = Config { configTimeout           = 10
                    , configOTPLength         = 6
                    , configOTPTimeoutSeconds = 10
                    , configTFARequired       = False
                    , configTwilio            = Nothing
                    , configUseTransactionLevels = False
                    , configEmail = Just testEmailConfig
                    }
  liftIO $ do
    _ <- runSqlPool (runMigrationSilent DB.migrateAll) pool
    f pool conf

withRunAPI :: ((forall a. API a -> IO a) -> IO b) -> IO b
withRunAPI f = withApiData $ \pool conf -> f $  runAPI pool conf

testApi :: API a -> IO a
testApi f = withRunAPI $ \run -> run f
