{-# LANGUAGE DeriveGeneric #-}
module Spotifyify.Manifest where

import System.FilePath (dropTrailingPathSeparator)
import Path
import Path.IO
import GHC.Generics
import Data.Yaml (ToJSON, toEncoding, encode, encodeFile)
import Data.Aeson.Types (genericToEncoding, defaultOptions)
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.String as U

type Album = String
data Artist = Artist { name :: String, albums :: [Album] } deriving Generic
data Manifest = Manifest { artists :: [Artist] } deriving Generic

instance ToJSON Artist where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Manifest where
  toEncoding = genericToEncoding defaultOptions

instance Show Manifest where
  show = U.decode . B.unpack . encode

buildManifest :: Path b Dir -> IO (Manifest)
buildManifest rootDir = do
  (dirs, files) <- listDirRel rootDir
  artists <- traverse (\d -> withCurrentDir rootDir (buildArtist d)) dirs
  pure $ Manifest artists

buildArtist :: Path Rel Dir -> IO (Artist)
buildArtist artistDir = do
  (dirs, files) <- listDirRel artistDir
  let albums = fmap dirToName dirs
  pure $ Artist (dirToName artistDir) albums

dirToName :: Path Rel Dir -> String
dirToName = dropTrailingPathSeparator . toFilePath

writeManifest :: Manifest -> FilePath -> IO ()
writeManifest manifest outputFile = encodeFile outputFile manifest
