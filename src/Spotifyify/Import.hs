{-# LANGUAGE OverloadedStrings #-}

{-|
  The logic for importing a library into Spotify based on a manifest.
-}
module Spotifyify.Import
  ( performImport
  ) where

import           Data.List            (find)
import           Data.Text            (Text, replace, strip, toLower, unpack)
import           Network.OAuth.OAuth2 (OAuth2Token)
import qualified Spotifyify.API       as API (Album (..), Artist (..),
                                              Artists (..), albumName,
                                              artistName, followArtist,
                                              getArtistAlbums, saveAlbums,
                                              searchArtists)
import           Spotifyify.Auth      (authenticate, initOAuth2Data)
import           Spotifyify.Log       (LogEntry (..), writeEntry)
import qualified Spotifyify.Manifest  as M (Artist (..), Manifest (..), name)
import           System.IO            (Handle)

-- |Import the given manifest into Spotify
performImport ::
     M.Manifest -- ^ the manifest describing your music collection
  -> Handle -- ^ handle to the log file
  -> IO ()
performImport (M.Manifest artists) logHandle = do
  oauth2Data <- initOAuth2Data
  oauth2Token <- authenticate oauth2Data
  sequence_ $ importArtist oauth2Token logHandle <$> artists

-- |Attempt to import the given artist into Spotify,
-- i.e. follow the artist and save all of the artist's albums that you have in your collection
importArtist :: OAuth2Token -> Handle -> M.Artist -> IO ()
importArtist oauth logHandle artist@(M.Artist name albums) = do
  result <- findArtist artist oauth
  case result of
    Just apiArtist -> do
      let artistName = unpack (API.artistName apiArtist)
      putStrLn $ "Found artist: " ++ artistName
      API.followArtist apiArtist oauth
      putStrLn $ "Followed " ++ artistName
      savedAlbums <- saveAlbums apiArtist albums oauth
      putStrLn $
        "Saved " ++ show (length savedAlbums) ++ " albums by " ++ artistName
      writeEntry logHandle (LogEntry name True (length savedAlbums))
    Nothing -> do
      putStrLn $ "Could not find artist in Spotify: " ++ unpack name
      writeEntry logHandle (LogEntry name False 0)

-- |Search for the artist on Spotify by name
findArtist :: M.Artist -> OAuth2Token -> IO (Maybe API.Artist)
findArtist artist oauth = searchFromOffset 0
  where
    searchFromOffset :: Int -> IO (Maybe API.Artist)
    searchFromOffset offset = searchAtOffset offset >>= handlePage
    searchAtOffset :: Int -> IO API.Artists
    searchAtOffset offset =
      API.searchArtists (normalise $ M.name artist) offset oauth
    findInPage :: API.Artists -> Maybe API.Artist
    findInPage (API.Artists [x] _ _ 1) = Just x
    findInPage (API.Artists items _ _ total) =
      find (matchesArtistName artist) items
    handlePage :: API.Artists -> IO (Maybe API.Artist)
    handlePage page =
      case findInPage page of
        Just artist -> return (Just artist)
        Nothing     -> giveUpOrContinue page
    giveUpOrContinue :: API.Artists -> IO (Maybe API.Artist)
    giveUpOrContinue (API.Artists _ _ offset total) =
      if offset + pageSize >= total
        then return Nothing -- give up
        else searchFromOffset (offset + pageSize) -- recurse

matchesArtistName :: M.Artist -> API.Artist -> Bool
matchesArtistName (M.Artist name _) (API.Artist _ name') =
  normalise name == normalise name'

normalise :: Text -> Text
normalise = strip . (replace "_" ".") . toLower

saveAlbums :: API.Artist -> [Text] -> OAuth2Token -> IO [API.Album]
saveAlbums artist albumNames oauth = do
  apiAlbums <- API.getArtistAlbums artist oauth
  let albumsToSave = filter shouldSave apiAlbums
  API.saveAlbums albumsToSave oauth
  return albumsToSave
  where
    shouldSave album =
      (normalise $ API.albumName album) `elem` normalisedAlbumNames
    normalisedAlbumNames = normalise <$> albumNames

pageSize :: Int
pageSize = 20
