module Main (main) where

import Bin
import CmdOptions (Options (..), runCmdOptions)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parser qualified
import Path

main :: IO ()
main = do
  (Options sourceCodePath) <- runCmdOptions
  putStrLn $ "FILEPATH: " <> sourceCodePath
  assemblyCode <- T.readFile sourceCodePath
  -- T.putStrLn assemblyCode
  -- T.putStrLn $ T.replicate 10 "-"
  mayStatements <- Parser.mainLocal assemblyCode
  case mayStatements of
    Nothing -> T.putStrLn "No results to be written"
    Just statements -> do
      writeBin sourceCodePath statements
