{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Twilio where

import           Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Conduit
import           Network.HTTP.Types

apiVersion :: BS.ByteString
apiVersion = "2010-04-01"

sendMessage :: Text
            -> Text
            -> Text
            -> Text
            -> Text
            -> IO (Either (Response BSL.ByteString) ())
sendMessage account authToken from to msg = do
    let accountSid = Text.encodeUtf8 account
        username = accountSid
        password = Text.encodeUtf8 authToken
    manager <- newManager tlsManagerSettings
    request' <- parseUrl "https://api.twilio.com/"
    let urlPath = BS.intercalate "/" [ ""
                                     , apiVersion
                                     , "Accounts"
                                     , accountSid
                                     , "Messages"
                                     ]
        body = [ ("From", Text.encodeUtf8 from)
               , ("To", Text.encodeUtf8 to)
               , ("Body", Text.encodeUtf8 msg)
               ]
        request = urlEncodedBody body $
                    request'{ path   = urlPath
                            , requestHeaders = [mkAuth username password
                                               ]
                            , checkStatus = \_status _rhdrs _cookies -> Nothing
                            }
    response <- httpLbs request manager
    liftIO $ print response
    case statusIsSuccessful $ responseStatus response of
     True -> return $ Right ()
     False -> return $ Left response
  where
    mkAuth username password  =
        ("Authorization", "Basic " <> (B64.encode $ username <> ":" <> password))
