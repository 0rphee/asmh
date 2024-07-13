module Main (main) where

import CmdOptions (Options (..), runCmdOptions)
import Path

main :: IO ()
main = do
  (Options sourceCodeFile) <- runCmdOptions
  putStrLn $ "FILEPATH: " <> toFilePath sourceCodeFile
