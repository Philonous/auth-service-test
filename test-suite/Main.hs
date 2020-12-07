{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Test.Tasty
import           Test.Tasty.Hspec

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64U
import           Data.Time.Clock            (NominalDiffTime)
import qualified Hedgehog                   as H
import qualified Hedgehog.Gen               as H
import qualified Hedgehog.Range             as H
import           Test.Hspec.Hedgehog
import           UnliftIO.Temporary

import           System.Process.Typed       (runProcess_, proc)

import qualified Headers                    as Headers
import qualified Nonce                      as Nonce
import qualified Sign                       as Sign

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

spec :: (Sign.PrivateKey, Sign.PublicKey) -> Spec
spec keys = do
  describe "signing" $ signSpec keys
  describe "nonces" $ nonceSpec
  describe "headers" $ headersSpec keys

--------------------------------------------------------------------------------
-- Signing and verifying -------------------------------------------------------
--------------------------------------------------------------------------------

-- | Change the message while keeping the signature intact
manipulatePayload :: ByteString -> ByteString -> ByteString
manipulatePayload signed mp =
  let sig = BS.take 86 signed
  in BS.concat [sig, ".", mp]

signSpec :: (Sign.PrivateKey, Sign.PublicKey) -> Spec
signSpec (priv, pub) = do
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

--------------------------------------------------------------------------------
-- Nonces ----------------------------------------------------------------------
--------------------------------------------------------------------------------

delta :: H.Gen NominalDiffTime
delta = realToFrac <$> -- positives deltas more likely than negative
                H.frequency [ (1, negate <$> H.double (H.exponentialFloat 0 5))
                            , (5, H.double (H.exponentialFloat 0 10))
                            ]

nonceSpec = do
  describe "updateCheckNonceFrame" $ do
    it "allows non-duplicate nonce" $ do
      let frame0 = Nonce.NonceFrame mempty 9000 mempty
          now = 9001
          (frame1, verdict1) = Nonce.updateCheckNonceFrame now 0 frame0
          (frame2, verdict2) = Nonce.updateCheckNonceFrame now 1 frame1
      verdict1 `shouldBe` Nonce.Accept
      frame1 `shouldBe` Nonce.NonceFrame mempty 9000 (Set.singleton 0)
      verdict2 `shouldBe` Nonce.Accept
      frame2 `shouldBe` Nonce.NonceFrame mempty 9000 (Set.fromList [0,1])

    it "detects duplicate nonces" $ do
      let frame0 = Nonce.NonceFrame mempty 9000 mempty
          now = 9001
          (frame1, verdict1) = Nonce.updateCheckNonceFrame now 0 frame0
          (frame2, verdict2) = Nonce.updateCheckNonceFrame now 0 frame1
      verdict1 `shouldBe` Nonce.Accept
      frame1 `shouldBe` Nonce.NonceFrame mempty 9000 (Set.singleton 0)
      verdict2 `shouldBe` Nonce.RejectSeen
      frame2 `shouldBe` frame1

    it "detects old duplicate nonces" $ do
      let frame0 = Nonce.NonceFrame mempty 9000 mempty
          now = 9001
          (frame1, verdict1) = Nonce.updateCheckNonceFrame now 0 frame0
          (frame2, verdict2) = Nonce.updateCheckNonceFrame (now + 5) 1 frame1
          (frame3, verdict3) = Nonce.updateCheckNonceFrame (now + 6) 0 frame2
      verdict1 `shouldBe` Nonce.Accept
      frame1 `shouldBe` Nonce.NonceFrame mempty 9000 (Set.singleton 0)
      -- after split
      verdict2 `shouldBe` Nonce.Accept
      frame2 `shouldBe` Nonce.NonceFrame (Set.fromList [0]) 9006 (Set.fromList [1])
      verdict3 `shouldBe` Nonce.RejectSeen
      frame3 `shouldBe` frame2


    it "rejects old nonces" $ do
      let frame0 = Nonce.NonceFrame mempty 9000 mempty
          now = 9001
          (frame1, verdict1) = Nonce.updateCheckNonceFrame (now + 10) 0 frame0
          (frame2, verdict2) = Nonce.updateCheckNonceFrame now 0 frame1
      verdict1 `shouldBe` Nonce.Accept
      frame1 `shouldBe` Nonce.NonceFrame mempty 9011 (Set.singleton 0)
      verdict2 `shouldBe` Nonce.RejectOld
      frame2 `shouldBe` frame1
      True `shouldBe` True
      return ()

    it "Cycles frames when necessary" $ do
      let frame0 = Nonce.NonceFrame mempty 9000 mempty
          (frame1, verdict1) = Nonce.updateCheckNonceFrame 9001 0 frame0
          (frame2, verdict2) = Nonce.updateCheckNonceFrame 9101 1 frame1
      frame1 `shouldBe` Nonce.NonceFrame mempty 9000 (Set.singleton 0)
      verdict1 `shouldBe` Nonce.Accept
      frame2 `shouldBe` Nonce.NonceFrame (Set.singleton 0) 9101 (Set.singleton 1)
      verdict2 `shouldBe` Nonce.Accept

--------------------------------------------------------------------------------
-- Header ----------------------------------------------------------------------
--------------------------------------------------------------------------------
headersSpec (privKey, pubKey) =
  describe "encodeHeaders / decodeHeaders" $ do
    it "should decode correctly signed headers" $ do
      pool <- liftIO Nonce.newNoncePool
      frame <- liftIO Nonce.newFrame
      encoded <- liftIO $ Headers.encodeHeaders privKey pool ("Hello" :: Text)
      decoded <- liftIO $ Headers.decodeHeaders pubKey frame encoded
      decoded `shouldBe` Right ("Hello" :: Text)
