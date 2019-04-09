{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotifyify.API where

import Data.ByteString.Char8 (pack)
import Data.Text (Text, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import Network.OAuth.OAuth2 (OAuth2Token, accessToken, atoken)
import Data.Aeson (FromJSON, ToJSON, toJSON, decodeStrict)
import GHC.Generics (Generic)

data Artist = Artist {
  id :: Text,
  uri :: Text,
  name :: Text,
  genres :: [Text]
} deriving (Show, Generic)

data Artists = Artists {
  items :: [Artist],
  limit :: Int,
  offset :: Int,
  total :: Int
} deriving (Show, Generic)

data ArtistSearchResult = ArtistSearchResult {
  artists :: Artists
} deriving (Show, Generic)

instance FromJSON Artist
instance FromJSON Artists
instance FromJSON ArtistSearchResult

searchArtists :: Text -> Int -> OAuth2Token -> IO ArtistSearchResult
searchArtists query offset = sendRequest request
  where
    request' = parseRequest "https://api.spotify.com/v1/search"
    addQueryParams =
      setRequestQueryString [
        ("q", Just $ encodeUtf8 query),
        ("type", Just "artist"),
        ("offset", Just $ (pack . show) offset)
      ]
    request = addQueryParams <$> request'

-- TODO check for 429 response -> sleep and retry
sendRequest :: FromJSON a => IO Request -> OAuth2Token -> IO a
sendRequest mkRequest oauth = do
  request' <- mkRequest
  let request = withAuth oauth request'
  response <- httpJSON request
  pure $ getResponseBody response

withAuth :: OAuth2Token -> Request -> Request
withAuth oauth req = addRequestHeader "Authorization" bearer req
  where
    bearer = encodeUtf8 $ intercalate " " [ "Bearer ", (atoken . accessToken) oauth ]
