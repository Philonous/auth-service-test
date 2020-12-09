{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Headers where

import           Control.Lens
import           Data.Aeson            ((.=))
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.TH         as Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Functor          ((<&>))
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word

import           JWS
import           Nonce
import           Sign

encodeHeaders ::
  Aeson.ToJSON payload => PrivateKey -> NoncePool -> payload -> IO JWS
encodeHeaders key noncePool payload = do
  nonce <- mkNonce noncePool
  now <- getPOSIXTime
  return $ signed key now nonce (BSL.toStrict $ Aeson.encode payload)

decodeHeaders ::
  Aeson.FromJSON payload =>
     PublicKey
  -> Frame
  -> JWS
  -> IO (Either String payload)
decodeHeaders key nonceFrame sig =
  case verified key sig of
    Nothing -> return $ Left "signature rejected"
    Just (header, encoded) ->
      case Aeson.decode' $ BSL.fromStrict encoded of
        Nothing -> return $ Left "failed to parse payload"
        Just payload -> do
          verdict <- handleNonce nonceFrame (fromIntegral $ header ^. t) (header ^. n)
          case verdict of
            RejectSeen -> return $ Left "nonce rejected: already used"
            RejectOld -> return $ Left "nonce rejected: too old"
            Accept -> return $ Right payload
