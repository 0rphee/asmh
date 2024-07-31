module Expr
  ( Register (..)
  , RawValue (..)
  , Label
  , Statement (..)
  , Operand (..)
  , Instruction (..)
  , Directive (..)
  )
where

import Data.Text (Text)
import Data.Word
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

data RawValue
  = W8 Word8
  | W16 Word16
  | IntOrChar Int
  deriving (Show, Eq)

type Label = Text

data Statement
  = Ins Instruction
  | Dir Directive
  | Lab Label
  deriving (Show, Eq)

data Operand
  = RegOp Register
  | ImmOp RawValue
  | MemOp Text
  -- TODO: proper memory addresses
  deriving (Show, Eq)

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

data Directive
  = ORG Word16
  | DB (Either (Text, [Word8]) [Word8])
  | END
  | NAME Text
  deriving (Show, Eq)
