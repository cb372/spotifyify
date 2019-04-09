{-# LANGUAGE DeriveGeneric #-}

module Spotifyify.Log where

import System.IO (Handle, hPutStrLn, readFile, hFlush)
import Data.Aeson (FromJSON, ToJSON, toJSON, decodeStrict)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.UTF8 as U (fromString)
import GHC.Generics (Generic)
import Path (Path, Abs, File, dirname, toFilePath)
import Path.IO (doesFileExist)

data LogEntry = LogEntry {
  artist :: Text,
  followed :: Bool,
  albumsImported :: Int
} deriving (Eq, Ord, Generic)

instance ToJSON LogEntry
instance FromJSON LogEntry

-- Reads the given log file, returning an empty list if the file does not exist
readLogFile :: Path Abs File -> IO [LogEntry]
readLogFile path = do
  exists <- doesFileExist path
  case exists of False -> return []
                 True ->  readLogFile' path

-- Reads the given log file. Assumes that the file exists.
readLogFile' :: Path Abs File -> IO [LogEntry]
readLogFile' path = do
  contents <- readFile $ toFilePath path
  let ls = lines contents
  return $ catMaybes $ fromString <$> ls

writeEntry :: Handle -> LogEntry -> IO ()
writeEntry h entry = do
  hPutStrLn h $ toString entry
  hFlush h

toString :: ToJSON a => a -> String
toString = unpack . toStrict . toLazyText . encodeToTextBuilder . toJSON

fromString :: FromJSON a => String -> Maybe a
fromString = decodeStrict . U.fromString
