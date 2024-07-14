module Expr where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (take)

type Parser = Parsec Void Text

data ProgramInfo = ProgramInfo
  { directives :: [Directive]
  , instructions :: [(String, Instruction)]
  }

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

type RawValue = (Either Word8 Word16)

type Label = Text

data Operand
  = RegOp Register
  | ImmOp RawValue
  | MemOp Text
  deriving (Show, Eq)

data Instruction
  = MOV Operand Operand
  | ADD Operand Operand
  | SUB Operand Operand
  | OR Operand Operand
  | INT Word8
  | JNS Label
  deriving (Show, Eq)

data Directive
  = ORG Word16
  | DB (Either (Text, [Word8]) [Word8])
  deriving (Show, Eq)

-- Parser for registers
parseRegister :: Parser Register
parseRegister =
  choice
    [ AX <$ string' "AX"
    , BX <$ string' "BX"
    , CX <$ string' "CX"
    , DX <$ string' "DX"
    , SI <$ string' "SI"
    , DI <$ string' "DI"
    , BP <$ string' "BP"
    , SP <$ string' "SP"
    , AL <$ string' "AL"
    , BL <$ string' "BL"
    , CL <$ string' "CL"
    , DL <$ string' "DL"
    , AH <$ string' "AH"
    , BH <$ string' "BH"
    , CH <$ string' "CH"
    , DH <$ string' "DH"
    ]

parseNum :: Num b => Int -> (Char -> Bool) -> Char -> Int -> Parser b
parseNum base cond ending numberOfDigits = try $ do
  bintxt <- takeP (Just $ show base <> " digits") numberOfDigits
  guard (T.all cond bintxt)
  satisfy (== ending)
  pure $ fromIntegral $ textToInt bintxt
  where
    textToInt :: Text -> Int
    textToInt = T.foldl (\acc x -> acc * base + digitToInt x) 0

parseBin :: Num b => Int -> Parser b
parseBin = parseNum 2 (\ch -> ch == '0' || ch == '1') 'b'

parseBin8 :: Parser Word8
parseBin8 = parseBin 8

parseBin12 :: Parser Word16
parseBin12 = parseBin 12

parseBin16 :: Parser Word16
parseBin16 = parseBin 16

parseHex :: Num b => Int -> Parser b
parseHex = parseNum 16 isHexDigit 'h'

parseHex8 :: Parser Word8
parseHex8 = parseHex 2

parseHex12 :: Parser Word16
parseHex12 = parseHex 3

parseHex16 :: Parser Word16
parseHex16 = parseHex 4

-- Parser for immediate values
parseImmediate8 :: Parser Word8
parseImmediate8 = parseBin8 <|> parseHex8 <|> L.decimal

parseImmediate12 :: Parser Word16
parseImmediate12 = parseBin12 <|> parseHex12 <|> L.decimal

parseImmediate16 :: Parser Word16
parseImmediate16 = parseBin16 <|> parseHex16 <|> L.decimal

-- Parser for memory operands (simplified, just accepting labels for now)
parseMemory :: Parser Text
parseMemory = T.pack <$> (char '[' *> some letterChar <* char ']')

parseVarOrLabelName :: Parser Text
parseVarOrLabelName = do
  (t, _) <- match (letterChar >> takeWhileP Nothing isAlphaNum)
  pure t

-- do

--  first <- letterChar
--  rest <- many (alphaNumChar <|> char '_')
--  return (first : rest)

-- Parser for operands
parseOperand :: Parser Operand
parseOperand =
  choice
    [ RegOp <$> try parseRegister
    , ImmOp <$> ((Left <$> parseImmediate8) <|> (Right <$> parseImmediate16))
    , MemOp <$> try parseMemory
    ]

-- Parser for instructions
parseInstruction :: Parser Instruction
parseInstruction =
  L.lexeme hspace $
    choice
      [ binaryP MOV "mov"
      , binaryP ADD "add"
      , binaryP SUB "sub"
      , binaryP OR "or"
      , INT
          <$> (L.symbol' hspace1 "int" *> (fromIntegral <$> (L.decimal :: Parser Int)))
      , JNS <$> do
          L.symbol' hspace1 "jns"
          parseVarOrLabelName
      ]
  where
    binaryP constr txt =
      constr
        <$> (L.symbol' hspace1 txt *> L.lexeme hspace parseOperand)
        <*> (char ',' *> hspace *> parseOperand)

parseDirective :: Parser Directive
parseDirective =
  choice
    [ ORG <$> (L.symbol' hspace1 "ORG" >> parseImmediate12)
    , do
        vname <-
          T.toLower
            <$> L.lexeme hspace (takeWhile1P (Just "variable char") isLetter)
              <?> "Variable name"
        L.symbol' hspace1 "db"
        v <- sepBy1 (L.lexeme hspace parseImmediate8) (char ',')
        when (vname == "db") $ fail "you cannot name a variable 'db'"
        pure $ DB $ Left (vname, v)
    , do
        L.symbol' hspace1 "db"
        v <- sepBy1 (L.lexeme hspace parseImmediate8) (char ',')
        pure $ DB $ Right v
    ]

-- Parser for a full line (instruction + optional label)
parseLine :: Parser (Either Directive (Maybe Label, Instruction))
parseLine = L.lexeme sc $ try (Left <$> parseDirective) <|> lbAndInstr
  where
    lbAndInstr =
      choice
        [ do
            labe <-
              optional . try $ L.lexeme hspace (parseVarOrLabelName <* char ':')
            instruction <- parseInstruction
            pure $ Right (labe, instruction)
        ]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

-- Main parser function
parseAssembly
  :: Text
  -> Either
      (ParseErrorBundle Text Void)
      [Either Directive (Maybe Label, Instruction)]
parseAssembly =
  parse
    (sc *> some parseLine <* eof)
    "myfile"

mainLocal :: Text -> IO ()
mainLocal assemblyCode = do
  case parseAssembly assemblyCode of
    -- case parseOnly parseLine assemblyCode of
    Left err -> do
      putStrLn "Error: "
      putStrLn $ errorBundlePretty err
    Right instructions -> traverse_ print instructions
