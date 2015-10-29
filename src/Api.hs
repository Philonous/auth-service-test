{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Api where

import Backend
import Control.Monad.Trans
import Database.Persist.Sql
import Servant
import Types
import Control.Monad.Trans.Either
import Network.Wai

type LoginAPI = "login"
              :> ReqBody '[JSON] Login
              :> Post '[JSON] B64Token

serveLogin :: ConnectionPool -> Server LoginAPI
serveLogin pool loginReq = loginHandler
  where
    loginHandler = do
        mbToken <- lift . runAPI pool $ login loginReq
        case mbToken of
         Just tok -> return tok
         Nothing -> left err403

type CheckTokenAPI = "checkToken"
                  :>  Capture "token" B64Token
                  :> Get '[JSON] (Headers '[Header "user" Username] ())

serveCheckToken :: ConnectionPool -> Server CheckTokenAPI
serveCheckToken pool token = checkTokenHandler
  where
    checkTokenHandler = do
        res <- lift . runAPI pool $ checkToken token
        case res of
         Nothing -> left err403
         Just usr -> return $ addHeader usr ()

apiPrx :: Proxy (LoginAPI :<|> CheckTokenAPI)
apiPrx = Proxy

serveAPI :: ConnectionPool -> Application
serveAPI pool = serve apiPrx $ serveLogin pool
                              :<|> serveCheckToken pool
