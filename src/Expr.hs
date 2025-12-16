module Expr
  ( Register (..)
  , RawValue (..)
  , Label
  , Statement (..)
  , Operand (..)
  , Instruction (..)
  , Directive (..)
  , ToHexString (..)
  )
where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Builder.Linear (Builder)
import Data.Text.Display
import Data.Word
import Numeric (showHex)
import Prelude hiding (take)

data Register
  = -- | (16 bit) the accumulator register (divided into AH / AL).
    AX
  | -- | (16 bit) the base address register (divided into BH / BL).
    BX
  | -- | (16 bit) the count register (divided into CH / CL).
    CX
  | -- | (16 bit) the data register (divided into DH / DL).
    DX
  | -- | (16 bit) source index register.
    SI
  | -- | (16 bit) destination index register.
    DI
  | -- | (16 bit) base pointer.
    BP
  | -- | (16 bit) stack pointer.
    SP
  | -- | (8 bit)
    AL
  | -- | (8 bit)
    BL
  | -- | (8 bit)
    CL
  | -- | (8 bit)
    DL
  | -- | (8 bit)
    AH
  | -- | (8 bit)
    BH
  | -- | (8 bit)
    CH
  | -- | (8 bit)
    DH
  deriving (Show, Eq)

instance Display Register where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    AX -> "AX"
    BX -> "BX"
    CX -> "CX"
    DX -> "DX"
    SI -> "SI"
    DI -> "DI"
    BP -> "BP"
    SP -> "SP"
    AL -> "AL"
    BL -> "BL"
    CL -> "CL"
    DL -> "DL"
    AH -> "AH"
    BH -> "BH"
    CH -> "CH"
    DH -> "DH"

data RawValue
  = RW8 Word8
  | RW16 Word16
  | RInt Int
  | RChar Char
  deriving (Show, Eq)

instance Display RawValue where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    RW8 w -> fromString (toHexString w) <> "b"
    RW16 w -> fromString (toHexString w) <> "b"
    RInt w -> displayBuilder w
    RChar w -> "'" <> displayBuilder w <> "'"

type Label = Text

data Statement
  = Ins Instruction
  | Dir Directive
  | Lab Label
  deriving (Show, Eq)

instance Display Statement where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    Ins x -> displayBuilder x
    Dir x -> displayBuilder x
    Lab x -> displayBuilder x

data Operand
  = RegOp Register
  | ImmOp RawValue
  | MemOp Text
  -- TODO: proper memory addresses
  deriving (Show, Eq)

instance Display Operand where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    RegOp x -> displayBuilder x
    ImmOp x -> displayBuilder x
    MemOp x -> "[" <> displayBuilder x <> "]"

data Instruction
  = MOV Operand Operand
  | ADD Operand Operand
  | SUB Operand Operand
  | OR Operand Operand
  | INT Word8
  | JNS Label
  | JMP Label -- TODO: or 8 byte instruction (FFFFh:AAAAh)
  | INC Operand
  | CMP Operand Operand
  | JE Label
  | RET
  deriving (Show, Eq)

instance Display Instruction where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    MOV x y -> dis "MOV " [x, y]
    ADD x y -> dis "ADD " [x, y]
    SUB x y -> dis "SUB " [x, y]
    OR x y -> dis "OR " [x, y]
    INT x -> dis "INT " [x]
    JNS x -> dis "JNS " [x]
    JMP x -> dis "JMP " [x]
    INC x -> dis "INC " [x]
    CMP x y -> dis "CMP " [x, y]
    JE x -> dis "JE " [x]
    RET -> dis @Operand "RET" []
    where
      dis :: Display x => Builder -> [x] -> Builder
      dis iname args = iname <> go (if null args then "" else " ") args
        where
          go accum [] = accum
          go accum (x : xs) = go (accum <> ", " <> displayBuilder x) xs

data Directive
  = ORG Word16
  | DB (Either (Text, [Word8]) [Word8])
  | END
  | NAME Text
  deriving (Show, Eq)

instance Display Directive where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    ORG w -> "ORG" <> displayBuilder (RW16 w)
    DB _ei -> "TODO NOT DONE"
    END -> "END"
    NAME txt -> "NAME \"" <> displayBuilder txt <> "\""

class ToHexString a where
  toHexString :: a -> String

instance ToHexString Word8 where
  toHexString w = pad2 $ showHex w ""
    where
      pad2 s = replicate (2 - length s) '0' ++ s

instance ToHexString Word16 where
  toHexString w = pad4 $ showHex w ""
    where
      pad4 s = replicate (4 - length s) '0' ++ s
