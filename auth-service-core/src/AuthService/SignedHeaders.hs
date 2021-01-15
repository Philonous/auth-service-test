{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Aeson               as Aeson
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.List                as List
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.TypeLits             (KnownSymbol)
import qualified Network.HTTP.Types       as HTTP
import           Network.Wai              (requestHeaders, Request)
import qualified Network.Wai              as Wai
import           Servant
import           Servant.Server
import           Servant.Server.Internal  (delayedFailFatal, DelayedIO, withRequest, addAuthCheck)

import qualified SignedAuth.Headers       as Headers
import qualified SignedAuth.Nonce         as Nonce
import qualified SignedAuth.Sign          as Sign

import           AuthService.Types
import qualified Data.Swagger.ParamSchema as Swagger
import qualified Servant.Swagger          as Swagger

import System.IO.Unsafe (unsafePerformIO)

-- | The context needed to create
data AuthContext =
  AuthContext
  { authContextPubKey ::  Sign.PublicKey
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

--------------------------------------------------------------------------------
-- Request handling ------------------------------------------------------------
--------------------------------------------------------------------------------
-- Factored out because both resolveAuthHeader and runAuthMaybe need it
handleRequest ctx req =
    case List.lookup "X-Auth" (requestHeaders req) of
      Nothing -> return Nothing
      Just authH -> do
        res <- liftIO (Headers.decodeHeaders (ctx ^. pubKey) (ctx ^. nonceFrame)
                 (Headers.JWS authH))
        case res of
          Left e -> do
            liftIO . logError $ "Error handling Authorization header: " ++ e
            return (Just $ Left e)
          Right r -> return $ Just (Right r)
  where
    logError e = ctx ^. logger $ "Authorization error: "  ++ e

--------------------------------------------------------------------------------
-- Middleware ------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Middleware = Application -> Application
-- Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- Middleware = (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived)
--            -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

type MiddlewareWith a = (a -> Application) -> Application

-- | Parse and verify authentication headers, pass the resolved value downstream
resolveAuthHeader :: AuthContext -> MiddlewareWith (Maybe AuthHeader)
resolveAuthHeader ctx downstream req respond = do
  handleRequest ctx req >>= \case
    Nothing -> next Nothing
    Just (Left e) -> err403
    Just (Right r) -> next $ Just r
  where
    next hdr =
      downstream hdr req respond
    err403 = respond $ Wai.responseLBS HTTP.status403 [] "Authentication denied"

--------------------------------------------------------------------------------
-- Servant ---------------------------------------------------------------------
--------------------------------------------------------------------------------

data AuthRequired = AuthRequired | AuthOptional

newtype AuthJWS (required :: AuthRequired) a = AuthJWS  a

makePrisms ''AuthJWS

instance Swagger.ToParamSchema (AuthJWS required a) where
  toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy String)

instance Swagger.HasSwagger rest => Swagger.HasSwagger (AuthJWS required a :> rest) where
  toSwagger _ = Swagger.toSwagger (Proxy :: Proxy (Header "X-Auth" String :> rest))

type instance IsElem' e (AuthJWS required a :> s) = IsElem e s

runAuth ::
     AuthContext
  -> Maybe AuthHeader
  -> Request
  -> DelayedIO AuthHeader
runAuth ctx Nothing req = do
  liftIO $ ctx ^. logger $ "Authorization error: Authorization header missing"
  delayedFailFatal err403
runAuth _ctx (Just authHeader) _ = do
  return authHeader

-- | Basic Authentication
instance ( HasServer api context
         , HasContextEntry context (Maybe AuthHeader)
         , HasContextEntry context AuthContext
         )
    => HasServer (AuthJWS 'AuthRequired AuthHeader :> api) context where

  type ServerT (AuthJWS 'AuthRequired AuthHeader :> api) m
    = AuthHeader -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
       authContext = getContextEntry context :: AuthContext
       authHeader = getContextEntry context :: (Maybe AuthHeader)
       authCheck = withRequest $ runAuth authContext authHeader

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- | Basic Authentication
instance ( HasServer api context
         , HasContextEntry context (Maybe AuthHeader)
         )
    => HasServer (AuthJWS 'AuthOptional AuthHeader :> api) context where

  type ServerT (AuthJWS 'AuthOptional AuthHeader :> api) m
    = Maybe AuthHeader -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
       authHeader = getContextEntry context :: Maybe AuthHeader
       authCheck = withRequest $ \_request -> return authHeader

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- | Create an auth header (for debugging / testing)
mkAuthHeader ::
     Nonce.NoncePool
  -> Sign.PrivateKey
  -> AuthHeader
  -> IO HTTP.Header
mkAuthHeader noncePool privKey authHeader = do
  Headers.JWS jws <-  Headers.encodeHeaders privKey noncePool authHeader
  return ("X-Auth", jws)

--------------------------------------------------------------------------------
-- Log Auth Information --------------------------------------------------------
--------------------------------------------------------------------------------
