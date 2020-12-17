{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Handling of signed headers in upstream application

module AuthService.SignedHeaders where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.Aeson              as Aeson
import qualified Data.List               as List
import           GHC.TypeLits            (KnownSymbol)
import           Network.Wai             (requestHeaders, Request)
import           Servant
import           Servant.Server
import           Servant.Server.Internal (delayedFailFatal, DelayedIO, withRequest, addAuthCheck)

import qualified SignedAuth.Headers      as Headers
import qualified SignedAuth.Nonce        as Nonce
import qualified SignedAuth.Sign         as Sign

import AuthService.Types
import qualified Data.Swagger.ParamSchema as Swagger

data AuthContext = AuthContext { authContextPubKey ::  Sign.PublicKey
                               , authContextNonceFrame :: Nonce.Frame
                               , authContextLogger :: String -> IO ()
                               }

makeLensesWith camelCaseFields ''AuthContext

-- | Create a new nonce frame, get the ambient logging function and create an
-- auth context
mkAuthContext :: MonadLoggerIO m => Sign.PublicKey -> m AuthContext
mkAuthContext pubKey = do
  frame <- liftIO Nonce.newFrame
  logfun <- askLoggerIO
  return AuthContext
    { authContextPubKey = pubKey
    , authContextNonceFrame = frame
    , authContextLogger = logfun defaultLoc "signed-auth" LevelInfo . toLogStr
    }

newtype AuthJWS a = AuthJWS a

instance Swagger.ToParamSchema (AuthJWS a) where
  toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy String)

runAuth ::
  Aeson.FromJSON a =>
     AuthContext
  -> Request
  -> DelayedIO a
runAuth ctx req = do
  case List.lookup "Authorization" $ requestHeaders req of
    Nothing -> do
      liftIO $ logError "Authorization header missing"
      delayedFailFatal err403
    Just authH -> liftIO (Headers.decodeHeaders (ctx ^. pubKey) (ctx ^. nonceFrame)
                           (Headers.JWS authH))
      >>= \case
        Left e -> do
          liftIO . logError $ "Error handling Authorization header: " ++ e
          delayedFailFatal err403
        Right r -> return r
  where
    logError e = ctx ^. logger $ "Authorization error: "  ++ e

-- | Basic Authentication
instance ( HasServer api context
         , HasContextEntry context AuthContext
         , Aeson.FromJSON a
         )
    => HasServer (AuthJWS a :> api) context where

  type ServerT (AuthJWS a :> api) m = a -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
       authContext = getContextEntry context
       authCheck = withRequest $ runAuth authContext

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
