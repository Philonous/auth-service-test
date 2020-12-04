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
import qualified Data.ByteString.Base64.URL as B64U
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

-- | Sign a bytestring and prepend the signature
-- key, input => base64_url_unpadded(sign(input,key)) ++ "." ++ input
signed :: PrivateKey -> ByteString -> ByteString
signed key input =
  let Signature sig = sign key input
  in BS.concat [B64U.encodeUnpadded sig, ".", input]

-- | Verify an octet sequence signed according to 'sign' and return the signed payload
verified :: PublicKey -> ByteString -> Maybe ByteString
verified pubKey signedBS =
  -- Base64: 3 octets become 4 base64 characters, padded to to full 4 characters
  -- ED25519 produces 64 signature octets
  -- 64 octets => /6*8 => 86 base64 characters + 2 paddings (dropped)
  let (base64Sig, rest) = BS.splitAt 86 signedBS
  in case BS.splitAt 1 rest of
       (".", payload) -> case B64U.decodeUnpadded base64Sig of
          Left{} -> Nothing
          Right sig -> case verifySignature pubKey payload (Signature sig) of
            False -> Nothing
            True -> Just payload
       _ -> Nothing
