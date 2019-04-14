{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotifyify.API where

import Prelude hiding (id, unwords)
import Data.ByteString.Char8 (pack, readInt)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, unwords)
import Data.Text.Encoding (encodeUtf8)
import Control.Concurrent (threadDelay)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
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

followArtist :: Artist -> OAuth2Token -> IO ()
followArtist artist = fireAndForget request
  where
    request' = parseRequest "PUT https://api.spotify.com/v1/me/following"
    addQueryParams =
      setRequestQueryString [
        ("ids", Just $ encodeUtf8 (id artist)),
        ("type", Just "artist")
      ]
    request = addQueryParams <$> request'

fireAndForget :: IO Request -> OAuth2Token -> IO ()
fireAndForget mkRequest oauth = do
  request' <- mkRequest
  let request = withAuth oauth request'
  response <- withRateLimiting $ httpNoBody request
  return ()

sendRequest :: FromJSON a => IO Request -> OAuth2Token -> IO a
sendRequest mkRequest oauth = do
  request' <- mkRequest
  let request = withAuth oauth request'
  response <- withRateLimiting $ httpJSON request
  pure $ getResponseBody response

withAuth :: OAuth2Token -> Request -> Request
withAuth oauth = addRequestHeader "Authorization" bearer
  where
    bearer = encodeUtf8 $ unwords [ "Bearer ", (atoken . accessToken) oauth ]

withRateLimiting :: IO (Response a) -> IO (Response a)
withRateLimiting sendRequest = do
  response <- sendRequest
  case getResponseStatus response of
    s | s == status429 -> do
      putStrLn $ "Hit the rate limiting. Sleeping for " ++ show (retryAfter response) ++ " seconds"
      threadDelay $ retryAfter response * 1000000
      withRateLimiting sendRequest
    _ -> return response

retryAfter :: Response a -> Int
retryAfter response = fromMaybe 5 headerValue
  where
    header = listToMaybe $ getResponseHeader "Retry-After" response
    headerValue = fst <$> (header >>= readInt)

