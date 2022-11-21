{-# LANGUAGE OverloadedStrings #-}
module SAML.AuthRequest where

import           Codec.Compression.Zlib.Raw  as Deflate
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Time.Clock             (getCurrentTime)
import           Data.Time.Format.ISO8601
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import           Network.HTTP.Types          (urlEncode)
import           Text.XML

import qualified Network.Wai.SAML2.Request   as AuthnRequest

mkRequest :: Text -> IO BS.ByteString
mkRequest issuer = do
  request <- AuthnRequest.issueAuthnRequest issuer

  let lbs = AuthnRequest.renderUrlEncodingDeflate request

  return $ "SAMLRequest=" <> lbs
