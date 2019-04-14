{-# LANGUAGE DeriveGeneric #-}

module Spotifyify.Log where

import           Data.Aeson             (FromJSON, ToJSON, decodeStrict, toJSON)
import           Data.Aeson.Text        (encodeToTextBuilder)
import qualified Data.ByteString.UTF8   as U (fromString)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text, unpack)
import           Data.Text.Encoding     (decodeUtf8)
import           Data.Text.Lazy         (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           GHC.Generics           (Generic)
import           Path                   (Abs, File, Path, dirname, toFilePath)
import           Path.IO                (doesFileExist)
import           System.IO              (Handle, hFlush, hPutStrLn, readFile)

data LogEntry = LogEntry
  { artist         :: Text
  , followed       :: Bool
  , albumsImported :: Int
  } deriving (Eq, Ord, Generic)

instance ToJSON LogEntry

instance FromJSON LogEntry

-- Reads the given log file, returning an empty list if the file does not exist
readLogFile :: Path Abs File -> IO [LogEntry]
readLogFile path = do
  exists <- doesFileExist path
  if exists
    then readLogFile' path
    else return []

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
