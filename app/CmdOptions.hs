{-# LANGUAGE TemplateHaskell #-}

module CmdOptions
  ( Options (..)
  , runCmdOptions
  )
where

import Data.Version (showVersion)
import GitHash
import Options.Applicative
import Paths_asmh (version)

newtype Options = Options
  { sourceCodeFile :: FilePath
  }

runCmdOptions :: IO Options
runCmdOptions = execParser options

options :: ParserInfo Options
options =
  info
    (opts <**> helper)
    ( fullDesc
        <> header headerString
        <> progDesc "i8086 assembler"
        <> footer
          "source code: https://github.com/0rphee/asmh"
        <> failureCode 64
    )
  where
    headerString = "asmh - v" <> versionString

opts :: Parser Options
opts =
  Options
    <$> strArgument
      ( metavar "FILENAME"
          <> help "Input file"
          <> action "file"
      )
      <**> simpleVersioner versionString

versionString :: String
versionString = showVersion version <> gitInfoString
  where
    gitInfoString = case $$tGitInfoCwdTry of
      Right gi ->
        concat
          [ " - "
          , giBranch gi
          , "@"
          , giHash gi
          ]
      _ -> ""
