{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Spotifyify.Manifest where

import GHC.Generics (Generic)
import qualified Data.Set as S (fromList, notMember)
import Data.Yaml (ToJSON, FromJSON, toEncoding, encode, encodeFile, decodeFileThrow)
import Data.Aeson.Types (genericToEncoding, defaultOptions)
import System.FilePath (dropTrailingPathSeparator)
import Path (Path, Abs, Dir, File, dirname, toFilePath)
import Path.IO (listDir)
import Spotifyify.Log (LogEntry, artist)

type Album = String
data Artist = Artist { name :: String, albums :: [Album] } deriving Generic
data Manifest = Manifest { artists :: [Artist] } deriving Generic

instance ToJSON Artist where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Manifest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Artist
instance FromJSON Manifest

instance Show Artist where
  show (Artist name albums) = name ++ " (" ++ show (length albums) ++ " albums)"

buildManifest :: Path Abs Dir -> IO (Manifest)
buildManifest rootDir = do
  (dirs, files) <- listDir rootDir
  artists <- traverse processArtistDir dirs
  let artistsWithAlbums = filter hasAtLeastOneAlbum artists
  pure $ Manifest artistsWithAlbums

processArtistDir :: Path Abs Dir -> IO (Artist)
processArtistDir artistDir = do
  let artistName = dirToName artistDir
  (dirs, files) <- listDir artistDir
  let albums = fmap dirToName dirs
  let artist = Artist artistName albums
  putStrLn $ "Processed artist: " ++ show artist
  pure artist

dirToName :: Path Abs Dir -> String
dirToName = dropTrailingPathSeparator . toFilePath . dirname

countArtists :: Manifest -> Int
countArtists (Manifest artists) = length artists

countAlbums :: Manifest -> Int
countAlbums (Manifest artists) = sum $ fmap albumsForArtist artists
  where albumsForArtist Artist{albums} = length albums

hasAtLeastOneAlbum :: Artist -> Bool
hasAtLeastOneAlbum Artist{albums = []} = False
hasAtLeastOneAlbum Artist{} = True

writeManifest :: Manifest -> Path Abs File -> IO ()
writeManifest manifest outputPath = encodeFile (toFilePath outputPath) manifest

readManifest :: Path Abs File -> IO Manifest
readManifest inputPath = decodeFileThrow (toFilePath inputPath)

-- Filter out any artists that are found in the logs
filterArtists :: Manifest -> [LogEntry] -> Manifest
filterArtists (Manifest artists) logs = Manifest filtered
  where
    filtered = filter nameNotInLogs artists
    nameNotInLogs artist = not $ elem (name artist) artistNamesInLogs
    artistNamesInLogs = S.fromList $ artist <$> logs

