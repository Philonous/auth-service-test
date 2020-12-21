{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Handling of signed headers in upstream application

module AuthService.SignedHeaders
  ( module AuthService.SignedHeaders
  , Sign.PublicKey
  , Sign.readPublicKeyDer
  , Sign.readPublicKeyPem
  , Nonce.Frame
  , Nonce.newFrame
   -- Encoding headers
  , Sign.PrivateKey
  , Sign.mkKeys
  , Sign.readPrivateKeyDer
  , Sign.readPrivateKeyPem
  , Headers.encodeHeaders
  ) where


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
import qualified Servant.Swagger as Swagger

data AuthContext = AuthContext { authContextPubKey ::  Sign.PublicKey
                               , authContextNonceFrame :: Nonce.Frame
                               , authContextLogger :: String -> IO ()
                               }

makeLensesWith camelCaseFields ''AuthContext

-- | Create an auth context
authContext ::
  Sign.PublicKey ->
  Nonce.Frame ->
  (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) ->
  AuthContext
authContext pubKey frame logfun =
  AuthContext
    { authContextPubKey = pubKey
    , authContextNonceFrame = frame
    , authContextLogger = logfun defaultLoc "signed-auth" LevelInfo . toLogStr
    }

-- | Create a new nonce frame, get the ambient logging function and create an
-- auth context
mkAuthContext :: MonadLoggerIO m => Sign.PublicKey -> m AuthContext
mkAuthContext pubKey = do
  frame <- liftIO Nonce.newFrame
  logfun <- askLoggerIO
  return $ authContext pubKey frame logfun

data AuthRequired = AuthRequired | AuthOptional

newtype AuthJWS (required :: AuthRequired) a = AuthJWS  a

makePrisms ''AuthJWS

instance Swagger.ToParamSchema (AuthJWS required a) where
  toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy String)

instance Swagger.HasSwagger rest => Swagger.HasSwagger (AuthJWS required a :> rest) where
  toSwagger _ = Swagger.toSwagger (Proxy :: Proxy (Header "X-Auth" String :> rest))

type instance IsElem' e (AuthJWS required a :> s) = IsElem e s

runAuth ::
  Aeson.FromJSON a =>
     AuthContext
  -> Request
  -> DelayedIO a
runAuth ctx req = do
  runAuthMaybe ctx req >>= \case
    Nothing -> do
      liftIO $ logError "Authorization header missing"
      delayedFailFatal err403
    Just x -> return x
  where
    logError e = ctx ^. logger $ "Authorization error: "  ++ e

runAuthMaybe ::
  Aeson.FromJSON a =>
     AuthContext
  -> Request
  -> DelayedIO (Maybe a)
runAuthMaybe ctx req = do
  case List.lookup "X-Auth" $ requestHeaders req of
    Nothing -> return Nothing
    Just authH -> liftIO (Headers.decodeHeaders (ctx ^. pubKey) (ctx ^. nonceFrame)
                           (Headers.JWS authH))
      >>= \case
        Left e -> do
          liftIO . logError $ "Error handling Authorization header: " ++ e
          delayedFailFatal err403
        Right r -> return $ Just r
  where
    logError e = ctx ^. logger $ "Authorization error: "  ++ e

-- | Basic Authentication
instance ( HasServer api context
         , HasContextEntry context AuthContext
         , Aeson.FromJSON a
         )
    => HasServer (AuthJWS 'AuthRequired a :> api) context where

  type ServerT (AuthJWS 'AuthRequired a :> api) m = a -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
       authContext = getContextEntry context
       authCheck = withRequest $ runAuth authContext

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- | Basic Authentication
instance ( HasServer api context
         , HasContextEntry context AuthContext
         , Aeson.FromJSON a
         )
    => HasServer (AuthJWS 'AuthOptional a :> api) context where

  type ServerT (AuthJWS 'AuthOptional a :> api) m = Maybe a -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
       authContext = getContextEntry context
       authCheck = withRequest $ runAuthMaybe authContext

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
