{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotifyify.Manifest where

import           Data.Aeson.Types (defaultOptions, genericToEncoding)
import qualified Data.Set         as S (fromList, notMember)
import           Data.Text        (Text, pack, unpack)
import           Data.Yaml        (FromJSON, ToJSON, decodeFileThrow, encode,
                                   encodeFile, toEncoding)
import           GHC.Generics     (Generic)
import           Path             (Abs, Dir, File, Path, dirname, toFilePath)
import           Path.IO          (listDir)
import           Spotifyify.Log   (LogEntry, artist, followed)
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

writeManifest :: Manifest -> Path Abs File -> IO ()
writeManifest manifest outputPath = encodeFile (toFilePath outputPath) manifest

readManifest :: Path Abs File -> IO Manifest
readManifest inputPath = decodeFileThrow (toFilePath inputPath)

-- Filter out any artists that are found in the logs and have already been followed
filterArtists :: Manifest -> [LogEntry] -> Manifest
filterArtists (Manifest artists) logs = Manifest filtered
  where
    filtered = filter nameNotInLogs artists
    nameNotInLogs artist = S.notMember (name artist) alreadyFollowedArtists
    alreadyFollowedArtists = S.fromList $ artist <$> alreadyFollowedLogs
    alreadyFollowedLogs = followed `filter` logs
