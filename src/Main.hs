module Main where

import Path (parseRelDir)
import Options.Applicative
import Data.Semigroup ((<>))
import Spotifyify.Manifest

data Cmd
  = BuildManifest String String
  | Import String

buildManifestParser :: Parser Cmd
buildManifestParser = BuildManifest <$>
  strOption (
    long "input-dir" <> short 'i' <> metavar "DIRECTORY" <> help "Root directory to search")
  <*>
  strOption (long "output-file" <> short 'o' <> metavar "FILE" <> value "manifest.yml" <> help "Output file")

importParser :: Parser Cmd
importParser = Import <$>
  strOption (long "input-file" <> short 'i' <> metavar "FILE" <> help "Input file")

cmdParser :: Parser Cmd
cmdParser =
  subparser (
    command "build-manifest" (info buildManifestParser ( progDesc "Find all your artists and albums and write them to a manifest file" ))
    <>
    command "import" (info importParser ( progDesc "Import the contents of a manifest file into Spotify" ))
  )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
     <> header "A tool for importing your local music files into Spotify" )

run :: Cmd -> IO ()
run (BuildManifest rootDir outputFile) = do
  -- TODO normalise the rootDir path
  rootPath <- parseRelDir rootDir
  manifest <- buildManifest rootPath
  -- TODO print stats about the manifest before writing to file
  writeManifest manifest outputFile
  putStrLn $ "Wrote manifest to file " ++ outputFile
run (Import manifestFile) = putStrLn "Importing into Spotify..."
