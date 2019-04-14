{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
  Client and models for the Spotify API
-}
module Spotifyify.API
  ( Artist(..)
  , Artists(..)
  , Album(..)
  , searchArtists
  , getArtistAlbums
  , followArtist
  , saveAlbums
  , albumName
  , artistName
  ) where

import           Control.Concurrent        (threadDelay)
import           Data.Aeson                (FromJSON, ToJSON, decodeStrict,
                                            toJSON)
import           Data.ByteString.Char8     (pack, readInt)
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Text                 (Text, intercalate, unpack, unwords)
import           Data.Text.Encoding        (encodeUtf8)
import           GHC.Generics              (Generic)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Network.OAuth.OAuth2      (OAuth2Token, accessToken, atoken)
import           Prelude                   hiding (id, unwords)

data Artist = Artist
  { id   :: Text
  , name :: Text
  } deriving (Show, Generic)

artistId Artist {id = x} = x

artistName Artist {name = x} = x

data Artists = Artists
  { items  :: [Artist]
  , limit  :: Int
  , offset :: Int
  , total  :: Int
  } deriving (Show, Generic)

data ArtistSearchResult = ArtistSearchResult
  { artists :: Artists
  } deriving (Show, Generic)

data Album = Album
  { id   :: Text
  , name :: Text
  } deriving (Show, Generic)

albumId Album {id = x} = x

albumName Album {name = x} = x

data ArtistAlbumsResult = ArtistAlbumsResult
  { items :: [Album]
  } deriving (Show, Generic)

instance FromJSON Artist

instance FromJSON Artists

instance FromJSON ArtistSearchResult

instance FromJSON Album

instance FromJSON ArtistAlbumsResult

-- |Search for artist by name
searchArtists ::
     Text -- ^ artist name to search for
  -> Int -- ^ offset to start at
  -> OAuth2Token -- ^ authentication token
  -> IO Artists
searchArtists query offset oauth = artists <$> sendRequest request oauth
  where
    request' = parseRequest "https://api.spotify.com/v1/search"
    addQueryParams =
      setRequestQueryString
        [ ("q", Just $ encodeUtf8 query)
        , ("type", Just "artist")
        , ("offset", Just $ (pack . show) offset)
        ]
    request = addQueryParams <$> request'

-- |Follow the given artist on Spotify
followArtist :: Artist -> OAuth2Token -> IO ()
followArtist Artist {id = artistId} = fireAndForget request
  where
    request' = parseRequest "PUT https://api.spotify.com/v1/me/following"
    addQueryParams =
      setRequestQueryString
        [("ids", Just (encodeUtf8 artistId)), ("type", Just "artist")]
    request = addQueryParams <$> request'

-- |Get all (actually the first 50) albums by the given artist
getArtistAlbums :: Artist -> OAuth2Token -> IO [Album]
getArtistAlbums Artist {id = artistId} oauth =
  getItems <$> sendRequest request oauth
  where
    request' =
      parseRequest $
      "https://api.spotify.com/v1/artists/" ++ unpack artistId ++ "/albums"
    addQueryParams =
      setRequestQueryString
        [("include_groups", Just "album"), ("limit", Just "50")]
    request = addQueryParams <$> request'
    getItems :: ArtistAlbumsResult -> [Album]
    getItems = items

-- |Save the given albums to your Spotify library
saveAlbums :: [Album] -> OAuth2Token -> IO ()
saveAlbums albums = fireAndForget request
  where
    request' = parseRequest "PUT https://api.spotify.com/v1/me/albums"
    addQueryParams = setRequestQueryString [("ids", Just (encodeUtf8 albumIds))]
    albumIds = intercalate "," (albumId <$> albums)
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
    bearer = encodeUtf8 $ unwords ["Bearer ", (atoken . accessToken) oauth]

withRateLimiting :: IO (Response a) -> IO (Response a)
withRateLimiting sendRequest = do
  response <- sendRequest
  case getResponseStatus response of
    s
      | s == status429 -> do
        putStrLn $
          "Hit the rate limiting. Sleeping for " ++
          show (retryAfter response) ++ " seconds"
        threadDelay $ retryAfter response * 1000000
        withRateLimiting sendRequest
    _ -> return response

retryAfter :: Response a -> Int
retryAfter response = fromMaybe 5 headerValue
  where
    header = listToMaybe $ getResponseHeader "Retry-After" response
    headerValue = fst <$> (header >>= readInt)
