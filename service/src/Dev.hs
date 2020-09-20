-- Helpers for developing this application (for ghci)

module Dev where

import Control.Monad.Logger
import Control.Monad.Trans
import Database.Persist.Postgresql
import Types

devConf :: Config
devConf = Config
  { configTimeout              = 3600
  , configOTPLength            = 1
  , configOTPTimeoutSeconds    = 3600
  , configTFARequired          = False
  , configOtp                  = Nothing
  , configUseTransactionLevels = False
  , configEmail                = Nothing
  , configAccountCreation      = AccountCreationConfig
    { accountCreationConfigEnabled = True
    , accountCreationConfigDefaultInstances = []
    }
  }



runApiDev :: ConnectionString -> API a -> IO a
runApiDev cstr m = runStderrLoggingT $withPostgresqlPool cstr 3 $ \pool -> do
  liftIO $ runAPI pool devConf m
