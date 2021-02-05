{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- Handling of signed headers in upstream application

module AuthService.SignedHeaders
  (
   -- * Verifying headers
    AuthContext(..)
  , mkAuthContext
  , Sign.PublicKey
  , Sign.readPublicKeyDer
  , Sign.readPublicKeyPem
  , Nonce.Frame
  , Nonce.newFrame
   -- * Wai Middleware
  , AuthHeader(..)
  , resolveAuthHeader
   -- * Logging
  , logRequestBasic
  , RequestLogUser(..)
  , RequestLog(..)
  -- * Servant
  , AuthCredentials(..)
  , AuthRequired(..)
  , IsRole(..)
  , NeedsRole
   -- * Encoding headers
  , Sign.PrivateKey
  , Sign.mkKeys
  , Sign.readPrivateKeyDer
  , Sign.readPrivateKeyPem
  , Headers.encodeHeaders
  , mkAuthHeader
  ) where


import           Control.Lens
import           Control.Monad
import qualified Control.Monad.Catch             as Ex
import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.TH                   as Aeson
import           Data.IORef
import qualified Data.List                       as List
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import qualified Data.Text.Encoding.Error        as Text
import           Data.Time.Clock
import qualified Data.UUID                       as UUID
import           GHC.TypeLits                    (symbolVal, Symbol, KnownSymbol)
import qualified Network.HTTP.Types              as HTTP
import           Network.Wai                     (requestHeaders, Request)
import qualified Network.Wai                     as Wai
import           Servant
import           Servant.Server.Internal         (delayedFailFatal, DelayedIO, withRequest, addAuthCheck)

import qualified SignedAuth.Headers              as Headers
import qualified SignedAuth.Nonce                as Nonce
import qualified SignedAuth.Sign                 as Sign

import           AuthService.Types
import qualified Data.Swagger.ParamSchema        as Swagger
import qualified Servant.Swagger                 as Swagger

import           Helpers
import           Servant.Server.Internal.Delayed (Delayed(..))

-- | The context needed to create
data AuthContext =
  AuthContext
  { authContextPubKey ::  Sign.PublicKey
  , authContextNonceFrame :: Nonce.Frame
  , authContextLogger :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  }

makeLensesWith camelCaseFields ''AuthContext

-- | Create a new nonce frame, get the ambient logging function and create an
-- auth context
mkAuthContext :: MonadLoggerIO m => Sign.PublicKey -> m AuthContext
mkAuthContext pubKey = do
  frame <- liftIO Nonce.newFrame
  logfun <- askLoggerIO
  return $ AuthContext pubKey frame logfun

--------------------------------------------------------------------------------
-- Request handling ------------------------------------------------------------
--------------------------------------------------------------------------------
-- Factored out because both resolveAuthHeader and runAuthMaybe need it
handleRequest :: (MonadIO m, Aeson.FromJSON b) =>
                 AuthContext -> Request -> m (Maybe (Either String b))
handleRequest ctx req =
    case List.lookup "X-Auth" (requestHeaders req) of
      Nothing -> return Nothing
      Just authH -> do
        res <- liftIO (Headers.decodeHeaders (ctx ^. pubKey) (ctx ^. nonceFrame)
                 (Headers.JWS authH))
        case res of
          Left e -> do
            liftIO $ logWarn e
            return (Just $ Left e)
          Right r -> return $ Just (Right r)
  where
    logWarn e = authContextLogger ctx defaultLoc "auth-service" LevelWarn
                   . toLogStr $ "Error handling authorization header: "  ++ e

--------------------------------------------------------------------------------
-- Middleware ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Parse and verify authentication headers, pass the resolved value downstream
--
-- Use like a 'Middleware' just with the "downstream" application taking another
-- argument
resolveAuthHeader ::
      AuthContext
  -> (Maybe AuthHeader -> Application)
  -> Application
resolveAuthHeader ctx downstream req respond = do
  handleRequest ctx req >>= \case
    Nothing -> next Nothing
    Just (Left _e) -> err403
    Just (Right r) -> next $ Just r
  where
    next hdr =
      downstream hdr req respond
    err403 = respond $ Wai.responseLBS HTTP.status403 [] "Authentication denied"

-- Logging middleware
---------------------

data RequestLogUser =
  RequestLogUser
  { requestLogUserName :: Text
  , requestLogUserEmail :: Text
  , requestLogUserId :: Text
  , requestLogUserRoles :: [Text]
  } deriving (Show)

Aeson.deriveJSON Aeson.defaultOptions
  {Aeson.fieldLabelModifier = dropPrefix "requestLogUser"} ''RequestLogUser

data RequestLog =
  RequestLog
  { requestLogTime :: UTCTime
  , requestLogMethod :: Text
  , requestLogPath :: Text
  , requestLogUser :: Maybe RequestLogUser
  , requestLogResponseStatus :: Int
  , requestLogResponseTimeMs :: Integer
  }

Aeson.deriveJSON Aeson.defaultOptions
  {Aeson.fieldLabelModifier = dropPrefix "requestLog"} ''RequestLog

-- | Log basic facts about an HTTP requests including resolved authentication
-- details as json
--
-- Fields:
--
--   [@time@]: ISO 861 time when the request was received
--   [@path@]: Request path
--   [@user@]: Information about the user making the request (if available)
--   [@reponse_status@]: Numeric HTTP response status
--   [@response_time_ms@]: Number of milliseconds taken to formulate a response
--
-- User has the following fields:
--
--   [@name@]: Name of the user
--   [@email@]: Email address
--   [@id@]: Unique user ID of the user
logRequestBasic ::
     AuthContext
  -> Maybe AuthHeader
  -> Wai.Middleware
logRequestBasic ctx mbAuthHeader next  req respond = do
  begin <- getCurrentTime
  rStatus <- newIORef Nothing
  handled <- Ex.try
             $ next req (\res -> writeIORef rStatus (Just $ Wai.responseStatus res)
                                 >> respond res)
  end <- getCurrentTime
  let runTime = round $ (end `diffUTCTime` begin)  * 1000
      -- NominalDiffTime is treated like seconds by conversion functions
      user = mbAuthHeader <&> \hdr ->
        RequestLogUser
        { requestLogUserName = hdr ^. name . _Name
        , requestLogUserEmail = hdr ^. email . _Email
        , requestLogUserId = hdr ^. userID . _UserID . to UUID.toText
        , requestLogUserRoles = hdr ^. roles
        }
  case handled of
    Left (e :: Ex.SomeException) -> do
      -- unhandled exception, ignore any set reqponse status
      log RequestLog
          { requestLogTime = begin
          , requestLogMethod = Text.decodeUtf8With Text.lenientDecode
                                 $ Wai.requestMethod req
          , requestLogPath = Text.decodeUtf8With Text.lenientDecode
                               $ Wai.rawPathInfo req
          , requestLogUser = user
          , requestLogResponseStatus = 500
          , requestLogResponseTimeMs = runTime
          }
      Ex.throwM e
    Right responseReceived -> do
      mbStatus <- readIORef rStatus
      statuscode <- case mbStatus of
        Nothing -> do
          -- This should not happen
          logLine "auth-service-core" LevelError
                  "Subhandler did not return response status"
          return 0
        Just s -> return $ HTTP.statusCode s
      log RequestLog
          { requestLogTime = begin
          , requestLogMethod = Text.decodeUtf8With Text.lenientDecode
                                 $ Wai.requestMethod req
          , requestLogPath = Text.decodeUtf8With Text.lenientDecode
                               $ Wai.rawPathInfo req
          , requestLogUser = user
          , requestLogResponseStatus = statuscode
          , requestLogResponseTimeMs = runTime
          }
      return responseReceived
  where
    logLine = authContextLogger ctx defaultLoc
    log = authContextLogger ctx defaultLoc "simple-request-json" LevelInfo
            . toLogStr . Aeson.encode

--------------------------------------------------------------------------------
-- Servant ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Parameter to 'AuthCredentials', see there.
data AuthRequired = AuthRequired | AuthOptional

-- | Servant combinator to fetch user credentials. If the parameter is
-- ''AuthRequired' the request will be denied with 403 if the user has not
-- provided credentials.
--
-- Example:
--
-- > type MyAPI = "posts" :> AuthCredentials 'AuthOptional :> GET '[ JSON ] [Post]
--
-- The handler should take an 'AuthHeader' if ''AuthRequired' is set and @Maybe
-- 'AuthHeader'@ for ''AuthOptional'
newtype AuthCredentials (required :: AuthRequired) a = AuthCredentials a

makePrisms ''AuthCredentials

instance Swagger.ToParamSchema (AuthCredentials required a) where
  toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy String)

instance Swagger.HasSwagger rest => Swagger.HasSwagger (AuthCredentials required a :> rest) where
  toSwagger _ = Swagger.toSwagger (Proxy :: Proxy (Header "X-Auth" String :> rest))

type instance IsElem' e (AuthCredentials required a :> s) = IsElem e s

runAuth ::
     AuthContext
  -> Maybe AuthHeader
  -> Request
  -> DelayedIO AuthHeader
runAuth ctx Nothing _req = do
  liftIO $ authContextLogger ctx defaultLoc "auth-service" LevelWarn
    "Authorization error: Authorization header missing"
  delayedFailFatal err403
runAuth _ctx (Just authHeader) _ = do
  return authHeader

-- | Basic Authentication
instance ( HasServer api context
         , HasContextEntry context (Maybe AuthHeader)
         , HasContextEntry context AuthContext
         )
    => HasServer (AuthCredentials 'AuthRequired AuthHeader :> api) context where

  type ServerT (AuthCredentials 'AuthRequired AuthHeader :> api) m
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
    => HasServer (AuthCredentials 'AuthOptional AuthHeader :> api) context where

  type ServerT (AuthCredentials 'AuthOptional AuthHeader :> api) m
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
-- Servant Combinators ---------------------------------------------------------
--------------------------------------------------------------------------------

class IsRole (t :: k) where
  -- | How to check if the user has the approriate role. Return "true" if the
  -- role is present.
  checkRole :: Proxy t -> [Text] -> Bool

instance KnownSymbol s => IsRole (s :: Symbol) where
  checkRole proxy roles =
    let wantedHeader = Text.pack $ symbolVal proxy
    in wantedHeader `List.elem` roles

-- | Combinator for checking for the presence of a role. Doesn't return anything.
--
-- Use it either with a Symbol (type-level string) or a custom value that has an
-- IsRole instance
--
-- Example:
--
-- > type myAPI = "users" :> NeedsRole "admin" :> Get '[JSON] User
--
-- Another example
--
-- > data MyRole = RoleAdmin | RoleUser
-- >
-- > instance IsRole 'RoleAdmin where
-- >   checkRole _ = List.elem "admin"
-- >
-- > instance IsRole 'RoleUser
-- >   checkRole _ = List.elem "user"
-- >
-- > type MyAPI = "users" :> NeedsRole 'RoleAdmin :> Get '[JSON] User
data NeedsRole (a :: k)

instance ( HasServer api context
         , IsRole r
         , HasContextEntry context (Maybe AuthHeader)
         )
    => HasServer (NeedsRole r :> api) context where
  type ServerT (NeedsRole r :> api) m
    = ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck_` authCheck)
    where
       mbAuthHeader = getContextEntry context :: Maybe AuthHeader
       authCheck =
         case mbAuthHeader of
           Nothing -> delayedFailFatal err401
           Just authHeader ->
             unless (checkRole (Proxy :: Proxy r) (authHeader ^. roles))
               $ delayedFailFatal err403
       -- Adds auth check that doesn't return anything
       addAuthCheck_ Delayed{..} new =
         Delayed
         { authD   = authD <* new
         , ..
         }

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s
