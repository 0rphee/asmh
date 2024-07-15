module Main (main) where

import CmdOptions (Options (..), runCmdOptions)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Expr qualified
import Path
import Bin

main :: IO ()
main = do
  (Options sourceCodeFile) <- runCmdOptions
  putStrLn $ "FILEPATH: " <> sourceCodeFile
  assemblyCode <- T.readFile "asm/colors.asm"
  -- let assemblyCode =
  --       T.unlines
  --         [ "start: MOV AX, 1234 ; commment ;; coomeeeent"
  --         , "; comment          SUB CX, DX"
  --         , "SUB AX, DX"
  --         , -- ]
  --           -- [
  --           "ORG 100h"
  --         , "MOV AL, 00000111b   ; AL = 7"
  --         , "OR  AL, 0           ; just set flags."
  --         , "JNS label1"
  --         , -- , "PRINT 'signed.'"
  --           "JMP exit"
  --         ]

  T.putStrLn assemblyCode
  T.putStrLn $ T.replicate 10 "-"
  mayStatements <- Expr.mainLocal assemblyCode
  case mayStatements of
    Nothing -> T.putStrLn "No results to be written"
    Just statements -> do
      writeBin statements

