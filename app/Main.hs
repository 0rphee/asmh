module Main (main) where

import Bin
import CmdOptions (Options (..), runCmdOptions)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parser qualified
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  (Options sourceCodePath) <- runCmdOptions
  putStrLn $ "FILEPATH: " <> sourceCodePath
  assemblyCode <- T.readFile sourceCodePath
  case Parser.parseAssembly sourceCodePath assemblyCode of
    Left e -> putStrLn $ errorBundlePretty e
    Right statements -> writeBin sourceCodePath statements
