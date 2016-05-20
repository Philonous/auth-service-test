-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Backend
import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Data.Monoid
import           Database.Persist.Sql
import           Network.Wai
import           Servant
import           Types

import           Logging


type LoginAPI = "login"
              :> ReqBody '[JSON] Login
              :> Post '[JSON] (Headers '[Header "X-Token" B64Token] ReturnLogin)

-- Will be transformed into X-Token header and token cookie by the nginx
serveLogin :: ConnectionPool -> Config -> Server LoginAPI
serveLogin pool conf loginReq = loginHandler
  where
    loginHandler = do
        mbReturnLogin <- lift . runAPI pool conf $ login loginReq
        case mbReturnLogin of
         Right rl -> return (addHeader (returnLoginToken rl) rl)
         Left LoginErrorOTPRequired ->
             throwError ServantErr{ errHTTPCode = 499
                                  , errReasonPhrase = "OTP required"
                                  , errBody =
                                    "{\"error\":\"One time password required\"}"
                                  , errHeaders = []
                                  }
         Left _e -> throwError err403

type LogoutAPI = "logout"
               :> Capture "token" B64Token
               :> Post '[JSON] ()

serveLogout :: ConnectionPool -> Config -> Server LogoutAPI
serveLogout pool conf tok = logoutHandler
  where
    logoutHandler = do
        lift . runAPI pool conf $ logOut tok

type DisableSessionsAPI = "disable-sessions"
                       :> Capture "token" B64Token
                       :> Post '[JSON] ()

serverDisableSessions :: ConnectionPool -> Config -> Server DisableSessionsAPI
serverDisableSessions pool conf tok = disableSessionsHandler
  where
    disableSessionsHandler = do
      lift . runAPI pool conf $ closeOtherSessions tok

type CheckTokenAPI = "check-token"
                  :> Capture "token" B64Token
                  :> Capture "instance" InstanceID
                  :> Get '[JSON] (Headers '[Header "X-User" UserID] ReturnUser)

serveCheckToken :: ConnectionPool -> Config -> Server CheckTokenAPI
serveCheckToken pool conf tok inst = checkTokenHandler
  where
    checkTokenHandler = do
        res <- lift . runAPI pool conf $ do
            logDebug $ "Checking token " <> showText tok
                       <> " for instance " <> showText inst
            runMaybeT $ do
              usr <- MaybeT $ checkToken tok
              _ <- MaybeT $ checkInstance inst usr
              return usr
        case res of
         Nothing -> throwError err403
         Just usr -> return $ (addHeader usr $ ReturnUser usr)

type PublicCheckTokenAPI = "check-token"
                        :> Capture "token" B64Token
                        :> Get '[JSON] ()

servePublicCheckToken :: ConnectionPool -> Config -> Server PublicCheckTokenAPI
servePublicCheckToken pool conf tok = checkTokenHandler
  where
    checkTokenHandler = do
        res <- lift . runAPI pool conf $ do
            logDebug $ "Checking token " <> showText tok
            checkToken tok
        case res of
         Nothing -> throwError err403
         Just _usr -> return ()


type GetUserInstancesAPI = "user-instances"
                         :> Capture "user" UserID
                         :> Get '[JSON] [ReturnInstance]

serveGetUserInstancesAPI :: ConnectionPool
                         -> Config
                         -> Server GetUserInstancesAPI
serveGetUserInstancesAPI pool conf usr =
  lift . runAPI pool conf $ getUserInstances usr


type GetUserInfoAPI = "user-info-by-token"
                    :> Capture "token" B64Token
                    :> Get '[JSON] ReturnUserInfo

serveGetUserInfoAPI :: ConnectionPool
                    -> Config
                    -> Server GetUserInfoAPI
serveGetUserInfoAPI pool conf tok =  do
  mbRUI <- lift . runAPI pool conf $ getUserInfo tok
  case mbRUI of
   Nothing -> throwError err403
   Just rui -> return rui

apiPrx :: Proxy (    LoginAPI
                :<|> CheckTokenAPI
                :<|> PublicCheckTokenAPI
                :<|> LogoutAPI
                :<|> DisableSessionsAPI
                :<|> GetUserInstancesAPI
                :<|> GetUserInfoAPI
                )
apiPrx = Proxy

serveAPI :: ConnectionPool -> Config -> Application
serveAPI pool conf = serve apiPrx $ serveLogin pool conf
                               :<|> serveCheckToken pool conf
                               :<|> servePublicCheckToken pool conf
                               :<|> serveLogout pool conf
                               :<|> serverDisableSessions pool conf
                               :<|> serveGetUserInstancesAPI pool conf
                               :<|> serveGetUserInfoAPI pool conf
