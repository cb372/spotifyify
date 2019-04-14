{-# LANGUAGE OverloadedStrings #-}

module Spotifyify.Import where

import           Data.List            (find)
import           Data.Text            (Text, replace, strip, toLower, unpack)
import           Network.OAuth.OAuth2 (OAuth2Token)
import qualified Spotifyify.API       as API (Artist, ArtistSearchResult (..),
                                              Artists (..), followArtist, name,
                                              searchArtists)
import           Spotifyify.Auth      (authenticate, initOAuth2Data)
import           Spotifyify.Log       (LogEntry (..), writeEntry)
import qualified Spotifyify.Manifest  as M (Artist (..), Manifest (..), name)
import           System.IO            (Handle)

performImport :: M.Manifest -> Handle -> IO ()
performImport (M.Manifest artists) logHandle = do
  oauth2Data <- initOAuth2Data
  oauth2Token <- authenticate oauth2Data
  sequence_ $ importArtist oauth2Token logHandle <$> artists

importArtist :: OAuth2Token -> Handle -> M.Artist -> IO ()
importArtist oauth logHandle artist@(M.Artist name albums) = do
  result <- findArtist artist oauth
  case result of
    Just a -> do
      putStrLn $ "Found artist: " ++ unpack (API.name a)
      API.followArtist a oauth
      putStrLn $ "Followed " ++ unpack (API.name a)
      -- TODO save albums
      writeEntry logHandle (LogEntry name True (length albums))
    Nothing -> do
      putStrLn $ "Could not find artist in Spotify: " ++ unpack name
      writeEntry logHandle (LogEntry name False 0)

findArtist :: M.Artist -> OAuth2Token -> IO (Maybe API.Artist)
findArtist artist oauth = searchFromOffset 0
  where
    searchFromOffset :: Int -> IO (Maybe API.Artist)
    searchFromOffset offset = searchAtOffset offset >>= handlePage
    searchAtOffset :: Int -> IO API.ArtistSearchResult
    searchAtOffset offset =
      API.searchArtists (normalise $ M.name artist) offset oauth
    findInPage :: API.ArtistSearchResult -> Maybe API.Artist
    findInPage (API.ArtistSearchResult (API.Artists [x] _ _ 1)) = Just x
    findInPage (API.ArtistSearchResult (API.Artists items _ _ total)) =
      find (matchesArtistName artist) items
    handlePage :: API.ArtistSearchResult -> IO (Maybe API.Artist)
    handlePage page =
      case findInPage page of
        Just artist -> return (Just artist)
        Nothing     -> giveUpOrContinue page
    giveUpOrContinue :: API.ArtistSearchResult -> IO (Maybe API.Artist)
    giveUpOrContinue (API.ArtistSearchResult (API.Artists _ _ offset total)) =
      if offset + pageSize >= total
        then return Nothing -- give up
        else searchFromOffset (offset + pageSize) -- recurse

matchesArtistName :: M.Artist -> API.Artist -> Bool
matchesArtistName x y = normalise (M.name x) == normalise (API.name y)

normalise :: Text -> Text
normalise = strip . (replace "_" ".") . toLower
pageSize :: Int
pageSize = 20
