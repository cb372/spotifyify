{-# LANGUAGE OverloadedStrings #-}

module Spotifyify.Auth where

import           Data.Aeson                        (FromJSON, ToJSON,
                                                    decodeStrict, toJSON)
import           Data.Aeson.Text                   (encodeToTextBuilder)
import           Data.ByteString                   (ByteString)
import           Data.Either                       (either)
import           Data.Maybe                        (catMaybes)
import qualified Data.Text                         as T (Text, pack, unpack)
import           Data.Text.Encoding                (decodeUtf8)
import           Data.Text.Lazy                    (toStrict)
import           Data.Text.Lazy.Builder            (toLazyText)
import           GHC.Generics                      (Generic)
import           Network.HTTP.Client               (newManager)
import           Network.HTTP.Client.TLS           (tlsManagerSettings)
import           Network.OAuth.OAuth2              (ExchangeToken (..),
                                                    OAuth2 (..), OAuth2Token,
                                                    appendQueryParams,
                                                    authorizationUrl)
import           Network.OAuth.OAuth2.HttpClient   (fetchAccessToken)
import           Network.OAuth.OAuth2.TokenRequest (Errors)
import           System.Environment                (getEnv)
import           URI.ByteString                    (URI, parseURI,
                                                    serializeURIRef',
                                                    strictURIParserOptions)
import           Web.Browser                       (openBrowser)

initOAuth2Data :: IO OAuth2
initOAuth2Data = do
  clientId <- T.pack <$> getEnv "SPOTIFY_CLIENT_ID"
  clientSecret <- T.pack <$> getEnv "SPOTIFY_CLIENT_SECRET"
  authorizationUrl <- toUri "https://accounts.spotify.com/authorize"
  tokenUrl <- toUri "https://accounts.spotify.com/api/token"
  callbackUrl <- toUri "https://cb372.github.io/spotifyify/callback.html"
  return $
    OAuth2 clientId clientSecret authorizationUrl tokenUrl (Just callbackUrl)
  where
    toUri :: ByteString -> IO URI
    toUri bytestring = liftEither $ parseURI strictURIParserOptions bytestring

authenticate :: OAuth2 -> IO OAuth2Token
authenticate oauth2Data = do
  openBrowser
    (T.unpack . decodeUtf8 $ serializeURIRef' authorizationUrlWithScope)
  putStrLn "Type your authorization code and press return:"
  code <- getLine
  mgr <- newManager tlsManagerSettings
  errorOrToken <- fetchAccessToken mgr oauth2Data (ExchangeToken $ T.pack code)
  liftEither errorOrToken
  where
    authorizationUrlWithScope =
      appendQueryParams
        [("scope" :: ByteString, scope)]
        (authorizationUrl oauth2Data)
    scope =
      "user-library-read user-library-modify user-follow-read user-follow-modify" :: ByteString

liftEither :: Show l => Either l r -> IO r
liftEither = either (fail . show) return
