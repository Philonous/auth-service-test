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

serveLogin :: ConnectionPool -> Config -> Server LoginAPI
serveLogin pool conf loginReq = loginHandler
  where
    loginHandler = do
        mbToken <- lift . runAPI pool conf $ login loginReq
        case mbToken of
         Just tok -> return tok
         Nothing -> left err403

type LogoutAPI = "logout"
               :> Capture "token" B64Token
               :> Post '[JSON] ()

serveLogout :: ConnectionPool -> Config -> Server LogoutAPI
serveLogout pool conf token = logoutHandler
  where
    logoutHandler = do
        lift . runAPI pool conf $ logOut token

type CheckTokenAPI = "checkToken"
                  :> Capture "token" B64Token
                  :> Get '[JSON] (Headers '[Header "user" Username] ())

serveCheckToken :: ConnectionPool -> Config -> Server CheckTokenAPI
serveCheckToken pool conf token = checkTokenHandler
  where
    checkTokenHandler = do
        res <- lift . runAPI pool conf $ checkToken token
        case res of
         Nothing -> left err403
         Just usr -> return $ addHeader usr ()

apiPrx :: Proxy (LoginAPI :<|> CheckTokenAPI :<|> LogoutAPI)
apiPrx = Proxy

serveAPI :: ConnectionPool -> Config -> Application
serveAPI pool conf = serve apiPrx $ serveLogin pool conf
                               :<|> serveCheckToken pool conf
                               :<|> serveLogout pool conf
