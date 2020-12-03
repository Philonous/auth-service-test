{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Sign where

import qualified Crypto.Error             as Crypto
import qualified Crypto.PubKey.Ed25519    as Ed25519
import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding       as ASN1
import           Data.ASN1.Prim
import qualified Data.ByteArray           as BA
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as Base64
import           Data.Char
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Numeric
import           System.IO

type PrivateKey = (Ed25519.SecretKey, Ed25519.PublicKey)
type PublicKey = Ed25519.PublicKey

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

parsePrivateKeyAsn1 :: [ASN1] -> Either String ByteString
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

readPrivateKeyPem :: ByteString -> Either String PrivateKey
readPrivateKeyPem pem = do -- Either String
  asn1 <- decodePrivateKeyPEM pem
  keyBytes <- parsePrivateKeyAsn1 asn1
  case Ed25519.secretKey keyBytes of
      Crypto.CryptoPassed r -> Right (r, Ed25519.toPublic r)
      Crypto.CryptoFailed e -> Left $ "Could not read binary private key: " <> show e


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

readPublicKeyPem :: ByteString -> Either String PublicKey
readPublicKeyPem pem = do
  asn1 <- decodePublicKeyPEM pem
  keyBytes <- parsePublicKeyAsn1 asn1
  case Ed25519.publicKey keyBytes of
      Crypto.CryptoPassed r -> Right r
      Crypto.CryptoFailed e -> Left $ "Could not read binary private key: " <> show e

newtype Signature = Signature ByteString

sign :: PrivateKey -> ByteString -> Signature
sign (secret, public) input =
  Signature $ BA.convert $ Ed25519.sign secret public input

verify :: PublicKey -> ByteString -> Signature -> Bool
verify pubkey bs (Signature sigbytes) =
  case Ed25519.signature sigbytes of
    Crypto.CryptoFailed e -> error $ show e
    Crypto.CryptoPassed sig -> Ed25519.verify pubkey bs sig

hex :: ByteString -> String
hex bs = concatMap toHex $ BS.unpack bs
  where
    toHex b = case showHex b [] of
                [c] -> ['0', c]
                [c1, c2] -> [c1, c2]
                _ -> error "hex"
test path = do
  publicKeyPem <- BS.readFile path
  let Right bytes = decodePublicKeyPEM publicKeyPem
      Right keybytes = parsePublicKeyAsn1 bytes
  print (BS.length keybytes, hex keybytes)

runMain = do
  privateKeyPem <- BS.readFile "key2.pem"
  privkey <- case readPrivateKeyPem privateKeyPem of
           Left e -> error e
           Right r -> return r
  publicKeyPem <- BS.readFile "pub2.pem"
  pubkey <- case readPublicKeyPem publicKeyPem of
           Left e -> error e
           Right r -> return r
  let signed = sign privkey (Text.encodeUtf8 "Hello encryption!")
      verified = verify pubkey "Hello encryption!" signed
  print verified

  return ()
