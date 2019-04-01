{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Spotifyify.Manifest where

import System.FilePath (dropTrailingPathSeparator)
import Path (Path, Abs, Dir, dirname, toFilePath)
import Path.IO (listDir)
import GHC.Generics (Generic)
import Data.Yaml (ToJSON, toEncoding, encode, encodeFile)
import Data.Aeson.Types (genericToEncoding, defaultOptions)

type Album = String
data Artist = Artist { name :: String, albums :: [Album] } deriving Generic
data Manifest = Manifest { artists :: [Artist] } deriving Generic

instance ToJSON Artist where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Manifest where
  toEncoding = genericToEncoding defaultOptions

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

writeManifest :: Manifest -> FilePath -> IO ()
writeManifest manifest outputFile = encodeFile outputFile manifest
