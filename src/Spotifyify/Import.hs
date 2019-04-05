{-# LANGUAGE DeriveGeneric #-}

module Spotifyify.Import where

import System.IO (Handle, hPutStrLn, readFile)
import Data.Aeson (FromJSON, ToJSON, toJSON, decodeStrict)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Path (Path, Abs, File, dirname, toFilePath)
import Path.IO (doesFileExist)
import Spotifyify.Manifest (Manifest(..), Artist(..))
import Spotifyify.Log (LogEntry(..), writeEntry)

performImport :: Manifest -> Handle -> IO ()
performImport (Manifest artists) logHandle = do
  sequence $ (importArtist logHandle) <$> artists
  return ()

importArtist :: Handle -> Artist -> IO ()
importArtist logHandle (Artist name albums) = writeEntry logHandle (LogEntry name (length albums))

