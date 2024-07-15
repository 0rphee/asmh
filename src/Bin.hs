module Bin where

import Data.ByteString qualified as B
import Data.Serialize.Put
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word
import Expr

-- serialize :: [Statement] -> ByteString
-- serialize =

trans :: Instruction -> Put
trans = \case
  MOV (RegOp AX) (ImmOp (Right w16)) -> do
    putWord8 0xb8 >> putWord16le w16
  ADD operand1 operand2 -> undefined
  SUB operand1 operand2 -> undefined
  OR operand1 operand2 -> undefined
  INT word -> undefined
  JNS label -> undefined
  JMP label -> undefined
  INC operand -> undefined
  CMP operand1 operand2 -> undefined
  JE label -> undefined

writeBin :: [Statement] -> IO ()
writeBin instr = do
  B.writeFile "test.com" test
  T.putStrLn "Output written to test.com"
  where
    -- bs = undefined
    bs = undefined

    test =
      runPut $
        trans $
          MOV (RegOp AX) (ImmOp (Right 3))
