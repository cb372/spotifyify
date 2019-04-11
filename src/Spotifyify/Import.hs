module Spotifyify.Import where

import Data.List (find)
import Data.Text (Text, unpack)
import Control.Concurrent (threadDelay)
import System.IO (Handle)
import Network.OAuth.OAuth2 (OAuth2Token)
import Spotifyify.Manifest (Manifest(..), Artist(..), name)
import Spotifyify.Log (LogEntry(..), writeEntry)
import Spotifyify.Auth (initOAuth2Data, authenticate)
import Spotifyify.API (ArtistSearchResult(..), Artists(..), searchArtists)
import qualified Spotifyify.API as API (Artist, name)

performImport :: Manifest -> Handle -> IO ()
performImport (Manifest artists) logHandle = do
  oauth2Data <- initOAuth2Data
  oauth2Token <- authenticate oauth2Data
  putStrLn $ show oauth2Token
  sequence $ (importArtist oauth2Token logHandle) <$> artists
  return ()

importArtist :: OAuth2Token -> Handle -> Artist -> IO ()
importArtist oauth logHandle artist@(Artist name albums) = do
  artist <- findArtist artist oauth
  case artist of
    Just a -> putStrLn $ "Found artist: " ++ (unpack $ API.name a)
    Nothing -> putStrLn $ "Could not find any artist to match " ++ show artist
  writeEntry logHandle (LogEntry name True (length albums))
  threadDelay 5000000

findArtist :: Artist -> OAuth2Token -> IO (Maybe API.Artist)
findArtist artist oauth = searchFromOffset 0
  where
    searchFromOffset :: Int -> IO (Maybe API.Artist)
    searchFromOffset offset = searchAtOffset offset >>= handlePage

    searchAtOffset :: Int -> IO ArtistSearchResult
    searchAtOffset offset = searchArtists (name artist) offset oauth

    findInPage :: ArtistSearchResult -> Maybe API.Artist
    findInPage (ArtistSearchResult (Artists [x] _ _ 1)) =
      Just x
    findInPage (ArtistSearchResult (Artists items _ _ total)) =
      find (matchesArtistName artist) items

    handlePage :: ArtistSearchResult -> IO (Maybe API.Artist)
    handlePage page = case findInPage page of
                        Just artist -> return (Just artist)
                        Nothing -> giveUpOrContinue page

    giveUpOrContinue :: ArtistSearchResult -> IO (Maybe API.Artist)
    giveUpOrContinue (ArtistSearchResult (Artists _ _ offset total)) =
      if offset + pageSize >= total
         then return Nothing -- give up
         else searchFromOffset (offset + pageSize) -- recurse

matchesArtistName :: Artist -> API.Artist -> Bool
matchesArtistName x y = (name x) == (API.name y)

pageSize :: Int
pageSize = 20
