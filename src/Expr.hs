module Expr
  ( Size (..)
  , SizedProof (..)
  , Register (..)
  , Label
  , Statement (..)
  , RawValue (..)
  , Operand (..)
  , SomeSized (..)
  , Instruction (..)
  , Directive (..)
  , ToHexString (..)
  , rewrapSized
  , validate8
  , validate16
  , haveCompatibleSizes
  )
where

import Bits.Show (showFiniteBits)
import Data.Coerce (coerce)
import Data.Kind
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Display
import Data.Text.Lazy.Builder (Builder)
import Data.Word
import Numeric (showHex)
import Prelude hiding (take)

data Size = SW8 | SW16

data Register s where
  -- | (16 bit) the accumulator register (divided into AH / AL).
  AX :: Register SW16
  -- | (16 bit) the base address register (divided into BH / BL).
  BX :: Register SW16
  -- | (16 bit) the count register (divided into CH / CL).
  CX :: Register SW16
  -- | (16 bit) the data register (divided into DH / DL).
  DX :: Register SW16
  -- | (16 bit) source index register.
  SI :: Register SW16
  -- | (16 bit) destination index register.
  DI :: Register SW16
  -- | (16 bit) base pointer.
  BP :: Register SW16
  -- | (16 bit) stack pointer.
  SP :: Register SW16
  -- | (8 bit)
  AL :: Register SW8
  -- | (8 bit)
  BL :: Register SW8
  -- | (8 bit)
  CL :: Register SW8
  -- | (8 bit)
  DL :: Register SW8
  -- | (8 bit)
  AH :: Register SW8
  -- | (8 bit)
  BH :: Register SW8
  -- | (8 bit)
  CH :: Register SW8
  -- | (8 bit)
  DH :: Register SW8

-- deriving (Show, Eq)

instance Display (Register s) where
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

type Label = Text

data Statement
  = Ins Instruction
  | Dir Directive
  | Lab Label

-- deriving (Show, Eq)

instance Display Statement where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    Ins x -> displayBuilder x
    Dir x -> displayBuilder x
    Lab x -> displayBuilder x

data RawValue s where
  RW8 :: Word8 -> RawValue SW8
  RW16 :: Word16 -> RawValue SW16
  RWS :: Int -> RawValue s

instance Display (RawValue s) where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    RW8 x -> displayBuilder x
    RW16 x -> displayBuilder x
    RWS x -> displayBuilder x

data Operand (s :: Size)
  = RegOp (Register s)
  | ImmOp (RawValue s)
  | MemOp Text

-- TODO: proper memory addresses
-- deriving (Show, Eq)

data SizedProof (s :: Size) where
  SP8 :: SizedProof SW8
  SP16 :: SizedProof SW16
  SPS :: SizedProof s

data SomeSized (sized :: Size -> Type) where
  SS :: SizedProof s -> sized s -> SomeSized sized

rewrapSized
  :: forall sized sized2
   . (forall (y :: Size). sized y -> sized2 y)
  -> SomeSized sized
  -> SomeSized sized2
rewrapSized con (SS s x) = SS s (con x)

haveCompatibleSizes
  :: SomeSized sized
  -> SomeSized sized
  -> (forall s1 s2. sized s1 -> sized s2 -> r)
  -> r
  -> r
haveCompatibleSizes (SS s1 x) (SS s2 y) f e = case (s1, s2) of
  (SP8, SP16) -> e
  (SP16, SP8) -> e
  _ -> f x y

validate8
  :: (forall s. sized s -> Maybe (sized SW8)) -> SomeSized sized -> Maybe (sized SW8)
validate8 fun (SS s x) = case s of
  SP8 -> Just x
  SP16 -> Nothing
  SPS -> fun x

validate16
  :: (forall s. sized s -> Maybe (sized SW16))
  -> SomeSized sized
  -> Maybe (sized SW16)
validate16 fun (SS s x) = case s of
  SP8 -> Nothing
  SP16 -> Just x
  SPS -> fun x

instance Display (Operand s) where
  {-# INLINE displayBuilder #-}
  displayBuilder = \case
    RegOp x -> displayBuilder x
    ImmOp x -> displayBuilder x
    MemOp x -> "[" <> displayBuilder x <> "]"

data Instruction where
  MOV :: (Operand s) -> (Operand s) -> Instruction
  ADD :: (Operand s) -> (Operand s) -> Instruction
  SUB :: (Operand s) -> (Operand s) -> Instruction
  OR :: (Operand s) -> (Operand s) -> Instruction
  INT :: Word8 -> Instruction
  JNS :: Label -> Instruction
  JMP :: Label -> Instruction -- TODO: or 8 byte instruction (FFFFh:AAAAh)
  JE :: Label -> Instruction
  INC :: (Operand s) -> Instruction
  CMP :: (Operand s) -> (Operand s) -> Instruction
  RET :: Instruction

-- deriving (Show, Eq)

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
    RET -> dis @() "RET" []
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
    ORG w -> "ORG" <> displayBuilder w
    END -> "END"
    NAME txt -> "NAME \"" <> displayBuilder txt <> "\""
    _ -> error "TODO NOT DONE"

class ToHexString a where
  toHexString :: a -> String

instance ToHexString Int where
  toHexString w = showHex w ""

instance ToHexString Word8 where
  toHexString w = pad2 $ showHex w ""
    where
      pad2 s = replicate (2 - length s) '0' ++ s

instance ToHexString Word16 where
  toHexString w = pad4 $ showHex w ""
    where
      pad4 s = replicate (4 - length s) '0' ++ s
