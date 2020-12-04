{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans
import qualified Data.List            as List
import qualified Test.Tasty
import           Test.Tasty.Hspec

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import           UnliftIO.Temporary

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
  where
    sh x xs = shell $ List.unwords (x:xs)



main :: IO ()
main = do
  test <- testSpec "signed-authorization" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = describe "sign/verify" $ do
  it "Works " $ do
    (priv, pub) <- liftIO readKeys
    Sign.verified pub (Sign.signed priv "hello world") `shouldBe` Just "hello world"
