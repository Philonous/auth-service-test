{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans
import qualified Data.List            as List
import qualified Test.Tasty
import           Test.Tasty.Hspec

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Hedgehog             as H
import qualified Hedgehog.Gen         as H
import qualified Hedgehog.Range       as H
import           Test.Hspec.Hedgehog
import           UnliftIO.Temporary
import qualified Data.ByteString.Base64.URL as B64U

import           System.Process.Typed (runProcess_, proc, shell)

import qualified Sign                 as Sign

readKeys :: IO (Sign.PrivateKey, Sign.PublicKey)
readKeys = withSystemTempDirectory "signed-auth-test." $ \path -> do
  let privPath = path ++ "/" ++ "privkey.pem"
      pubPath = path ++ "/" ++ "pubkey.pem"
  runProcess_ $ proc "openssl"
    ["genpkey", "-algorithm", "ed25519", "-outform", "PEM", "-out", privPath]
  runProcess_ $ proc "openssl"
    ["pkey", "-in", privPath
    , "-outform", "PEM", "-pubout"
    , "-out", pubPath
    ]
  privKeyBytes <- BS.readFile privPath
  privKey <- case Sign.readPrivateKeyPem privKeyBytes of
               Left e -> error $ "Could not read private key: " ++ e
               Right r -> return r

  pubKeyBytes <- BS.readFile pubPath
  pubKey <- case Sign.readPublicKeyPem pubKeyBytes of
               Left e -> error $ "Could not read public key: " ++ e
               Right r -> return r
  return (privKey, pubKey)

main :: IO ()
main = do
  keys <- readKeys
  test <- testSpec "signed-authorization" (spec keys)
  Test.Tasty.defaultMain test

-- | Change the message while keeping the signature intact
manipulatePayload :: ByteString -> ByteString -> ByteString
manipulatePayload signed mp =
  let sig = BS.take 86 signed
  in BS.concat [sig, ".", mp]

spec :: (Sign.PrivateKey, Sign.PublicKey) -> Spec
spec (priv, pub) = do
  describe "manipulatePayload" $ do
    it "Keeps the signature intact" $ hedgehog $ do
      payload <- H.forAll $ H.bytes (H.linear 0 128)
      let signed = Sign.signed priv payload
          manipulated = manipulatePayload signed payload
      manipulated H.=== signed
  describe "sign/verify" $ do
    it "Verifies signed messages" $ hedgehog $ do
      payload <- H.forAll $ H.bytes (H.linear 0 128)
      H.tripping payload (Sign.signed priv) (Sign.verified pub)

    it "Does not verify bogus signature" $ hedgehog $ do
      payload <- H.forAll $ H.bytes (H.linear 0 128)
      bogusSig <- H.forAll $ H.bytes (H.singleton 64)
      let bogusMessage = BS.concat [ B64U.encodeUnpadded bogusSig
                                   , "."
                                   , payload
                                   ]
      Sign.verified pub bogusMessage H.=== Nothing

    it "Does not verify manipulated messages" $ hedgehog $ do
      payload <- H.forAll $ H.bytes (H.linear 0 128)
      newPayload <- H.forAll $ H.filter (/= payload) $ H.bytes (H.linear 0 128)

      let sig = Sign.signed priv payload
          bogus = manipulatePayload sig newPayload
      Sign.verified pub bogus H.=== Nothing
