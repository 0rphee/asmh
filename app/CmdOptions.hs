{-# LANGUAGE TemplateHaskell #-}

module CmdOptions
  ( Options (..)
  , runCmdOptions
  )
where

import Data.Version (showVersion)
import GitHash
import OptEnvConf
import Path
import Paths_asmh (version)

newtype Options = Options
  { sourceCodeFile :: Path Abs File
  }

instance HasParser Options where
  settingsParser = do
    val <-
      filePathSetting
        [ help "the file to compile"
        , argument
        ]
    pure $ Options val

runCmdOptions :: IO Options
runCmdOptions = runSettingsParser version descriptionString
  where
    descriptionString = concat ["Run asmh - v", showVersion version, gitInfoString]

    gitInfoString = case $$tGitInfoCwdTry of
      Right gi ->
        concat
          [ " - "
          , giBranch gi
          , "@"
          , giHash gi
          , " ("
          , show $ giCommitCount gi
          , " commits in HEAD"
          , ")"
          ]
      _ -> ""
