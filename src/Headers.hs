{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Headers where

import           Data.Aeson           ((.=))
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.TH        as Aeson
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ((<&>))
import           Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word


import           Nonce
import           Sign

data Envelope payload =
  Envelope { time_stamp :: Word32
           , nonce :: Nonce
           , payload :: payload
           }

Aeson.deriveJSON Aeson.defaultOptions ''Envelope

encodeHeaders ::
  Aeson.ToJSON payload => PrivateKey -> NoncePool -> payload -> IO ByteString
encodeHeaders key noncePool payload = do
  nonce <- mkNonce noncePool
  now <- getPOSIXTime <&> round
  let envelope = Aeson.encode
        Envelope { time_stamp = now
                 , nonce = nonce
                 , payload = payload
                 }
  return $ signed key (BSL.toStrict envelope)

decodeHeaders ::
  Aeson.FromJSON payload =>
     PublicKey
  -> Frame
  -> ByteString
  -> IO (Either String payload)
decodeHeaders key nonceFrame sig =
  case verified key sig of
    Nothing -> return $ Left "signature rejected"
    Just encoded ->
      case Aeson.decode' $ BSL.fromStrict encoded of
        Nothing -> return $ Left "failed to parse envelope"
        Just env -> do
          verdict <- handleNonce nonceFrame (fromIntegral $ time_stamp env) (nonce env)
          case verdict of
            Reject -> return $ Left "nonce rejected"
            Accept -> return $ Right (payload env)
