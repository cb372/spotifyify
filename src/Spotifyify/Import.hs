module Spotifyify.Import where

import Data.Text (Text)
import Control.Concurrent (threadDelay)
import System.IO (Handle)
import Network.OAuth.OAuth2 (OAuth2Token)
import Spotifyify.Manifest (Manifest(..), Artist(..))
import Spotifyify.Log (LogEntry(..), writeEntry)
import Spotifyify.Auth (initOAuth2Data, authenticate)
import Spotifyify.API (searchArtists)

performImport :: Manifest -> Handle -> IO ()
performImport (Manifest artists) logHandle = do
  oauth2Data <- initOAuth2Data
  oauth2Token <- authenticate oauth2Data
  putStrLn $ show oauth2Token
  sequence $ (importArtist oauth2Token logHandle) <$> artists
  return ()

importArtist :: OAuth2Token -> Handle -> Artist -> IO ()
importArtist oauth2 logHandle (Artist name albums) = do
  searchResult <- searchArtists name 0 oauth2
  putStrLn $ show searchResult
  writeEntry logHandle (LogEntry name True (length albums))
  threadDelay 5000000

findArtist :: Artist -> OAuth2Token -> IO (Maybe Text)
findArtist artist oauth =
  -- TODO keep requesting more search pages while total > offset + page size
  -- and a result with an exactly matching name has not been found
  return Nothing

