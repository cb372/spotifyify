module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Path                (parseRelDir, toFilePath)
import           Path.IO             (resolveDir', resolveFile')
import           Spotifyify.Import   (performImport)
import           Spotifyify.Log      (readLogFile)
import           Spotifyify.Manifest (artists, buildManifest, countAlbums,
                                      countArtists, filterArtists, readManifest,
                                      writeManifest)
import           System.IO           (IOMode (AppendMode), withFile)

data Cmd
  = BuildManifest String
                  String
  | Import String
           String

buildManifestParser :: Parser Cmd
buildManifestParser =
  BuildManifest <$>
  strOption
    (long "input-dir" <> short 'i' <> metavar "DIRECTORY" <>
     help "Root directory to search") <*>
  strOption
    (long "output-file" <> short 'o' <> metavar "FILE" <> value "manifest.yml" <>
     help "Output file (manifest)")

importParser :: Parser Cmd
importParser =
  Import <$>
  strOption
    (long "input-file" <> short 'i' <> metavar "FILE" <> value "manifest.yml" <>
     help "Input file (manifest)") <*>
  strOption
    (long "log-file" <> short 'l' <> metavar "FILE" <> value "spotifyify.log" <>
     help "Log file")

cmdParser :: Parser Cmd
cmdParser =
  subparser
    (command
       "build-manifest"
       (info
          buildManifestParser
          (progDesc
             "Find all your artists and albums and write them to a manifest file")) <>
     command
       "import"
       (info
          importParser
          (progDesc "Import the contents of a manifest file into Spotify")))

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (cmdParser <**> helper)
        (fullDesc <>
         header "A tool for importing your local music files into Spotify")

run :: Cmd -> IO ()
run (BuildManifest rootDir outputFile) = do
  absRootDir <- resolveDir' rootDir
  outputPath <- resolveFile' outputFile
  manifest <- buildManifest absRootDir
  putStrLn $
    "Found " ++
    show (countAlbums manifest) ++
    " albums by " ++ show (countArtists manifest) ++ " artists"
  writeManifest manifest outputPath
  putStrLn $ "Wrote manifest to file " ++ show outputPath
run (Import manifestFile logFile) = do
  putStrLn "Loading manifest"
  manifestPath <- resolveFile' manifestFile
  manifest <- readManifest manifestPath
  putStrLn $
    "Loaded manifest: " ++
    show (countAlbums manifest) ++
    " albums by " ++ show (countArtists manifest) ++ " artists"
  putStrLn "Loading log file"
  logPath <- resolveFile' logFile
  logs <- readLogFile logPath
  putStrLn $ "Loaded " ++ show (length logs) ++ " log entries"
  let filteredManifest = filterArtists manifest logs
  putStrLn $
    "Manifest after filtering: " ++
    show (countAlbums filteredManifest) ++
    " albums by " ++ show (countArtists filteredManifest) ++ " artists"
  putStrLn "Importing into Spotify..."
  withFile (toFilePath logPath) AppendMode (performImport filteredManifest)
  putStrLn "Done!"
