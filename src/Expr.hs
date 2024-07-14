module Expr where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word
import Debug.Trace
-- import Text.Megaparsec
import Prelude hiding (take)

-- type Parser = Parsec Void Text

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

data Operand
  = RegOp Register
  | ImmOp Word16
  | MemOp String
  deriving (Show, Eq)

data Instruction
  = MOV Operand Operand
  | ADD Operand Operand
  | SUB Operand Operand
  | INT Word8
  deriving (Show, Eq)

-- Parser for comment
skipComment :: Parser ()
skipComment = (skip (/= '%') <?> "help") *> skipWhile (/= '\n') <?> "skipComment"

-- Parser for registers
parseRegister :: Parser Register
parseRegister =
  choice
    [ AX <$ string "AX"
    , BX <$ string "BX"
    , CX <$ string "CX"
    , DX <$ string "DX"
    , SI <$ string "SI"
    , DI <$ string "DI"
    , BP <$ string "BP"
    , SP <$ string "SP"
    , AL <$ string "AL"
    , BL <$ string "BL"
    , CL <$ string "CL"
    , DL <$ string "DL"
    , AH <$ string "AH"
    , BH <$ string "BH"
    , CH <$ string "CH"
    , DH <$ string "DH"
    ]

parseNum :: Num b => Int -> (Char -> Bool) -> Char -> Int -> Parser b
parseNum base cond ending numberOfDigits = do
  bintxt <- take numberOfDigits
  guard (T.all cond bintxt)
  skip (== ending)
  pure $ fromIntegral $ textToInt bintxt
  where
    textToInt :: Text -> Int
    textToInt = T.foldl (\acc x -> acc * base + digitToInt x) 0

parseBin :: Num b => Int -> Parser b
parseBin = parseNum 2 (\ch -> ch == '0' || ch == '1') 'b'

parseBin8 :: Parser Word8
parseBin8 = parseBin 8

parseBin16 :: Parser Word16
parseBin16 = parseBin 16

parseHex :: Num b => Int -> Parser b
parseHex = parseNum 16 isHexDigit 'h'

parseHex8 :: Parser Word8
parseHex8 = parseHex 2

parseHex16 :: Parser Word16
parseHex16 = parseHex 4

-- Parser for immediate values
parseImmediate8 :: Parser Word8
parseImmediate8 = parseHex8 <|> parseBin8 <|> decimal

parseImmediate16 :: Parser Word16
parseImmediate16 = parseHex16 <|> parseBin16 <|> decimal

-- Parser for memory operands (simplified, just accepting labels for now)
parseMemory :: Parser String
parseMemory = char '[' *> many1 letter <* char ']'

-- Parser for operands
parseOperand :: Parser Operand
parseOperand =
  choice
    [ RegOp <$> parseRegister
    , ImmOp <$> parseImmediate16
    , MemOp <$> parseMemory
    ]

-- Parser for instructions
parseInstruction :: Parser Instruction
parseInstruction =
  choice
    [ MOV
        <$> (string "MOV" *> skipSpace *> parseOperand)
        <*> (char ',' *> skipSpace *> parseOperand)
    , ADD
        <$> (string "ADD" *> skipSpace *> parseOperand)
        <*> (char ',' *> skipSpace *> parseOperand)
    , SUB
        <$> (string "SUB" *> skipSpace *> parseOperand)
        <*> (char ',' *> skipSpace *> parseOperand)
    , INT
        <$> (string "INT" *> skipSpace *> (fromIntegral <$> (decimal :: Parser Int)))
    ]

-- Parser for a full line (instruction + optional label)
parseLine :: Parser (Maybe String, Instruction)
parseLine = do
  label <- optional (many1 letter <* char ':' <* skipSpace)
  instruction <- parseInstruction
  pure (label, instruction)

-- Main parser function
parseAssembly :: T.Text -> Either String [(Maybe String, Instruction)]
parseAssembly =
  parseOnly $
    sepBy'
      parseLine
      skipUnsignificantCharacters
  where
    skipUnsignificantCharacters =
      scan
        False
        ( \st c ->
            ( if st
                then (if isEndOfLine c then Just False else Just True)
                else
                  ( if c == ';'
                      then Just True
                      else if isEndOfLine c || isSpace c then Just False else Nothing
                  )
            )
        )

-- (many' (skip isEndOfLine <|> skipSpace *> skipComment))

-- <* endOfInput

mainLocal :: Text -> IO ()
mainLocal assemblyCode = do
  case parseAssembly assemblyCode of
    -- case parseOnly parseLine assemblyCode of
    Left err -> putStrLn $ "Error: " ++ err
    Right instructions -> print instructions
