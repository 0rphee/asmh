{-# LANGUAGE OverloadedRecordDot #-}

module Bin (writeBin) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Map (Map)
import Data.Map qualified as M
import Data.Serialize.Put
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import Debug.Trace
import Expr
import System.FilePath

type Location = Int

data ProgramInfo
  = ProgramInfo
  { directives :: Vector Directive
  , labels :: Map Label Location
  , instructions :: Vector Instruction
  }
  deriving (Eq, Show)

getInstOffset :: Instruction -> Int
getInstOffset = \case
  MOV (RegOp AX) (ImmOp _ew) -> 3
  MOV (RegOp BX) (ImmOp _ew) -> 3
  MOV (RegOp CX) (ImmOp _ew) -> 3
  MOV (RegOp AH) (ImmOp _ew) -> 2
  MOV (RegOp AL) (ImmOp _ew) -> 2
  MOV (RegOp BH) (ImmOp _ew) -> 2
  MOV (RegOp BL) (ImmOp _ew) -> 2
  MOV (RegOp DH) (ImmOp _ew) -> 2
  MOV (RegOp DL) (ImmOp _ew) -> 2
  INT _word -> 2
  JMP _label -> 2
  JE _label -> 2
  INC (RegOp _) -> 2
  CMP (RegOp DH) (ImmOp _ew) -> 3
  CMP (RegOp DL) (ImmOp _ew) -> 3
  RET -> 1
  x -> error $ "Instruction unimplemented: " <> show x

trans :: Map Label Location -> Location -> Instruction -> Put
trans labelMap instrLoc = \case
  MOV (RegOp AX) (ImmOp ew) -> putWord8 0xb8 >> putAsW16 ew
  MOV (RegOp BX) (ImmOp ew) -> putWord8 0xbb >> putAsW16 ew
  MOV (RegOp CX) (ImmOp ew) -> putWord8 0xb9 >> putAsW16 ew
  -- MOV (RegOp DX) (ImmOp ew) -> putWord8 >> putEitherW ew
  MOV (RegOp AH) (ImmOp ew) -> putWord8 0xb4 >> putAsW8 ew
  MOV (RegOp AL) (ImmOp ew) -> putWord8 0xb0 >> putAsW8 ew
  MOV (RegOp BH) (ImmOp ew) -> putWord8 0xb7 >> putAsW8 ew
  MOV (RegOp BL) (ImmOp ew) -> putWord8 0xb3 >> putAsW8 ew
  MOV (RegOp DH) (ImmOp ew) -> putWord8 0xb6 >> putAsW8 ew
  MOV (RegOp DL) (ImmOp ew) -> putWord8 0xb2 >> putAsW8 ew
  -- ADD operand1 operand2 -> undefined
  -- SUB operand1 operand2 -> undefined
  -- OR operand1 operand2 -> undefined
  INT word -> putWord8 0xcd >> putWord8 word
  JMP label ->
    putWord8 0xeb
      >> putWord8 (getOffset (checkLabel label) instrLoc)
  JE label ->
    putWord8 0x74
      >> putWord8 (getOffset (checkLabel label) instrLoc)
  -- JNS label -> undefined
  INC (RegOp DH) -> putWord8 0xfe >> putWord8 0xc6
  INC (RegOp BL) -> putWord8 0xfe >> putWord8 0xc3
  INC (RegOp DL) -> putWord8 0xfe >> putWord8 0xc2
  CMP (RegOp DH) (ImmOp ew) -> putWord8 0x80 >> putWord8 0xfe >> putAsW8 ew
  CMP (RegOp DL) (ImmOp ew) -> putWord8 0x80 >> putWord8 0xfa >> putAsW8 ew
  RET -> putWord8 0xc3
  x -> error $ "Instruction unimplemented: " <> show x
  where
    getOffset :: Location -> Location -> Word8
    getOffset locationOfLabelDeclaration locationOfCurrJumpInstr =
      fromIntegral $
        locationOfLabelDeclaration - locationOfCurrJumpInstr

    checkLabel :: Label -> Location
    checkLabel label = case M.lookup label labelMap of
      Nothing -> error $ concat ["Label '", T.unpack label, "' not found'"]
      Just v -> v
    putAsW8 :: RawValue -> Put =
      putWord8 . \case
        W8 w -> w
        W16 w -> fromIntegral w
        IntOrChar w -> fromIntegral w

    putAsW16 :: RawValue -> Put =
      putWord16le . \case
        W8 w -> fromIntegral w
        W16 w -> w
        IntOrChar w -> fromIntegral w

firstPass :: [Statement] -> ProgramInfo
firstPass ls = go ls 0 (ProgramInfo mempty mempty mempty)
  where
    go :: [Statement] -> Int -> ProgramInfo -> ProgramInfo
    go [] _offset accum = accum
    go (x : xs) offset acc = case x of
      Ins newInstr ->
        let newOffset = offset + getInstOffset newInstr
         in go xs newOffset $ acc {instructions = V.snoc acc.instructions newInstr}
      Dir newDir -> go xs offset $ acc {directives = V.snoc acc.directives newDir}
      Lab newLabel ->
        case xs of
          (Ins _ : _) ->
            let newInstrPos = offset
             in go xs offset $ acc {labels = M.insert newLabel newInstrPos acc.labels}
          _ -> error "there must be a label after an instruction"

secondPass :: ProgramInfo -> ByteString
secondPass = runPut . go 0
  where
    go offset p = case V.uncons p.instructions of
      Just (nextInstruction, rest) -> do
        let nextInstructionIndex = offset + getInstOffset nextInstruction
        trans p.labels nextInstructionIndex nextInstruction
        go nextInstructionIndex (p {instructions = rest})
      Nothing -> pure ()

writeBin :: FilePath -> [Statement] -> IO ()
writeBin originalFileName instr = do
  let newFileName = takeBaseName originalFileName <> ".com"
  let binaryFile = secondPass $ firstPass instr
  B.writeFile newFileName binaryFile
  T.putStrLn $ "Output written to " <> T.pack newFileName
