{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Sign where

import qualified Crypto.Error               as Crypto
import qualified Crypto.PubKey.Ed25519      as Ed25519
import qualified Data.ASN1.BinaryEncoding   as ASN1
import qualified Data.ASN1.Encoding         as ASN1
import           Data.ASN1.Prim
import qualified Data.ByteArray             as BA
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as Base64

import           Data.Char
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Numeric
import           System.IO

type PrivateKey = (Ed25519.SecretKey, Ed25519.PublicKey)
type PublicKey = Ed25519.PublicKey

-- | Read .pem-wrapped DER octet sequence of a private key
decodePrivateKeyPEM :: ByteString -> Either String [ASN1]
decodePrivateKeyPEM input = case BS.split (fromIntegral $ ord '\n') input of
  (   "-----BEGIN PRIVATE KEY-----"
    : b64Key
    : "-----END PRIVATE KEY-----"
    : _
    ) -> do
    binaryKey <- Base64.decode b64Key
    case ASN1.decodeASN1' ASN1.DER binaryKey of
      Left e -> Left $ show e
      Right r -> Right r
  _ -> Left "Could not parse .pem data"

-- | Parse private key binary data from ASN1 abstract type
parsePrivateKeyAsn1 :: [ASN1] -> Either String ByteString
-- ED25519 sequence deduced from looking at key OpenSSL produces
parsePrivateKeyAsn1
  [ Start Sequence
  , IntVal 0
  , Start Sequence
  , OID [1,3,101,112]
  , End Sequence
  , OctetString payload
  , End Sequence
  ] = Right (BS.drop 2 payload)
parsePrivateKeyAsn1 _ = Left "Could not parse ASN.1 key data"

-- | Read a private key from .pem
readPrivateKeyPem :: ByteString -> Either String PrivateKey
readPrivateKeyPem pem = do -- Either String
  asn1 <- decodePrivateKeyPEM pem
  keyBytes <- parsePrivateKeyAsn1 asn1
  case Ed25519.secretKey keyBytes of
      Crypto.CryptoPassed r -> Right (r, Ed25519.toPublic r)
      Crypto.CryptoFailed e -> Left $ "Could not read binary private key: " <> show e

-- | Read .pem-wrapped DER octet sequence of a public key
decodePublicKeyPEM :: ByteString -> Either String [ASN1]
decodePublicKeyPEM input = case BS.split (fromIntegral $ ord '\n') input of
  (   "-----BEGIN PUBLIC KEY-----"
    : b64Key
    : "-----END PUBLIC KEY-----"
    : _
    ) -> do
    binaryKey <- Base64.decode b64Key
    case ASN1.decodeASN1' ASN1.DER binaryKey of
      Left e -> Left $ show e
      Right r -> Right r
  _ -> Left "Could not parse .pem data"

-- | Parse public key binary data from ASN1 abstract type
parsePublicKeyAsn1 :: [ASN1] -> Either String ByteString
parsePublicKeyAsn1
  [ Start Sequence
  , Start Sequence
  , OID [1,3,101,112]
  , End Sequence
  , BitString payload
  , End Sequence
  ] = Right (BS.drop 1 $ putBitString payload)
parsePublicKeyAsn1 _ = Left "Could not parse ASN.1 key data"

-- | Read a public key from .pem
readPublicKeyPem :: ByteString -> Either String PublicKey
readPublicKeyPem pem = do
  asn1 <- decodePublicKeyPEM pem
  keyBytes <- parsePublicKeyAsn1 asn1
  case Ed25519.publicKey keyBytes of
      Crypto.CryptoPassed r -> Right r
      Crypto.CryptoFailed e -> Left $ "Could not read binary private key: " <> show e

newtype Signature = Signature ByteString

-- | Sign a ByteString
sign :: PrivateKey -> ByteString -> Signature
sign (secret, public) input =
  Signature $ BA.convert $ Ed25519.sign secret public input

-- | Check the signature of a ByteString
verifySignature :: PublicKey -> ByteString -> Signature -> Bool
verifySignature pubkey bs (Signature sigbytes) =
  case Ed25519.signature sigbytes of
    Crypto.CryptoFailed e -> error $ show e
    Crypto.CryptoPassed sig -> Ed25519.verify pubkey bs sig

mkKeys :: IO (PrivateKey, PublicKey)
mkKeys = do
  secret <- Ed25519.generateSecretKey
  let pub = Ed25519.toPublic secret
  return ((secret, pub), pub)
