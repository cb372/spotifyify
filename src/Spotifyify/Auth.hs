{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotifyify.Auth where

import System.Environment (getEnv)
import Data.Aeson (FromJSON, ToJSON, toJSON, decodeStrict)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Either (either)
import Data.Maybe (catMaybes)
import Data.ByteString (ByteString)
import qualified Data.Text as T (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.OAuth.OAuth2 (OAuth2(..), OAuth2Token, authorizationUrl)
import Network.OAuth.OAuth2.TokenRequest (Errors)
import URI.ByteString (URI, parseURI, strictURIParserOptions, serializeURIRef')
import Web.Browser (openBrowser)

initOAuth2Data :: IO OAuth2
initOAuth2Data = do
  clientId <- T.pack <$> getEnv "SPOTIFY_CLIENT_ID"
  clientSecret <- T.pack <$> getEnv "SPOTIFY_CLIENT_SECRET"
  authorizationUrl <- toUri "https://accounts.spotify.com/authorize"
  tokenUrl <- toUri "https://accounts.spotify.com/api/token"
  callbackUrl <- toUri "http://localhost:12345/callback"
  return $ OAuth2 clientId clientSecret authorizationUrl tokenUrl (Just callbackUrl)
  where
    toUri :: ByteString -> IO URI
    toUri bytestring = either (fail . show) return $ parseURI strictURIParserOptions bytestring

authenticate :: OAuth2 -> IO OAuth2Token
authenticate oauth2Data = do
  openBrowser (T.unpack . decodeUtf8 . serializeURIRef' $ authorizationUrl oauth2Data)
  fail "nope"
