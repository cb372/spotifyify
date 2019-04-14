{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
  A manifest is a list of all the artists, and their albums, which you want to
  import into your Spotify library.  The manifest is built by traversing the
  directories under a given root directory, under the assumption that the
  directory is structured as follows:

@
  root-dir/
    artist name 1/
      album name 1/
        track1.mp3
        ...
      album name 2/
        track1.mp3
        ...
    artist name 2/
      album name 1/
        ...
@

  TODO we could improve on this by attempting to read IDv3 tags from files
  inside the directories.
-}
module Spotifyify.Manifest
  ( Manifest(..)
  , Artist(..)
  , buildManifest
  , readManifest
  , writeManifest
  , countAlbums
  , countArtists
  , filterArtists
  ) where

import           Data.Aeson.Types (defaultOptions, genericToEncoding)
import qualified Data.Set         as S (fromList, notMember)
import           Data.Text        (Text, pack, unpack)
import           Data.Yaml        (FromJSON, ToJSON, decodeFileThrow, encode,
                                   encodeFile, toEncoding)
import           GHC.Generics     (Generic)
import           Path             (Abs, Dir, File, Path, dirname, toFilePath)
import           Path.IO          (listDir)
import           Spotifyify.Log   (LogEntry (..))
import           System.FilePath  (dropTrailingPathSeparator)

type Album = Text

data Artist = Artist
  { name   :: Text
  , albums :: [Album]
  } deriving (Generic)

data Manifest = Manifest
  { artists :: [Artist]
  } deriving (Generic)

instance ToJSON Artist where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Manifest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Artist

instance FromJSON Manifest

instance Show Artist where
  show (Artist name albums) = unpack $ name <> " (" <> albumCount <> " albums)"
    where
      albumCount = pack $ show (length albums)

-- |Build a manifest describing the artists and albums in the given directory.
buildManifest :: Path Abs Dir -> IO Manifest
buildManifest rootDir = do
  (dirs, files) <- listDir rootDir
  artists <- traverse processArtistDir dirs
  let artistsWithAlbums = filter hasAtLeastOneAlbum artists
  pure $ Manifest artistsWithAlbums

processArtistDir :: Path Abs Dir -> IO Artist
processArtistDir artistDir = do
  let artistName = dirToName artistDir
  (dirs, files) <- listDir artistDir
  let albums = fmap dirToName dirs
  let artist = Artist artistName albums
  putStrLn $ "Processed artist: " ++ show artist
  pure artist

dirToName :: Path Abs Dir -> Text
dirToName = pack . dropTrailingPathSeparator . toFilePath . dirname

countArtists :: Manifest -> Int
countArtists (Manifest artists) = length artists

countAlbums :: Manifest -> Int
countAlbums (Manifest artists) = sum $ fmap albumsForArtist artists
  where
    albumsForArtist Artist {albums} = length albums

hasAtLeastOneAlbum :: Artist -> Bool
hasAtLeastOneAlbum Artist {albums = []} = False
hasAtLeastOneAlbum Artist {}            = True

-- |Write the given manifest to a file as YAML
writeManifest :: Manifest -> Path Abs File -> IO ()
writeManifest manifest outputPath = encodeFile (toFilePath outputPath) manifest

-- |Read a manifest from the given YAML file
readManifest :: Path Abs File -> IO Manifest
readManifest inputPath = decodeFileThrow (toFilePath inputPath)

-- |Filter out any artists that are found in the logs and have already been followed
filterArtists :: Manifest -> [LogEntry] -> Manifest
filterArtists (Manifest artists) logs = Manifest filtered
  where
    filtered = filter nameNotInLogs artists
    nameNotInLogs artist = S.notMember (name artist) alreadyFollowedArtists
    alreadyFollowedArtists = S.fromList $ artist <$> alreadyFollowedLogs
    alreadyFollowedLogs = followed `filter` logs
