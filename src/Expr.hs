module Expr where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Foldable (traverse_)
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug
import Prelude hiding (take)

type Parser = Parsec Void Text

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

parseChar :: Num a => Parser a
parseChar = fromIntegral . ord <$> between "'" "'" anySingle

parseChar8 :: Parser Word8
parseChar8 = parseChar

parseChar12 :: Parser Word16
parseChar12 = parseChar

parseChar16 :: Parser Word16
parseChar16 = parseChar

-- Parser for immediate values
parseImmediate8 :: Parser Word8
parseImmediate8 =
  label "expecting 8bit number" $
    choice
      [ parseBin8
      , parseHex8
      , L.lexeme space1 L.decimal
      , parseChar8
      ]

parseImmediate12 :: Parser Word16
parseImmediate12 =
  label "expecting 12bit number" $
    choice
      [ parseBin12
      , parseHex12
      , L.lexeme space1 L.decimal
      , parseChar12
      ]

parseImmediate16 :: Parser Word16
parseImmediate16 =
  label "expecting 16bit number" $
    choice
      [ parseBin16
      , parseHex16
      , L.lexeme space1 L.decimal
      , parseChar16
      ]

-- Parser for memory operands (simplified, just accepting labels for now)
parseMemory :: Parser Text
parseMemory = T.pack <$> (char '[' *> some letterChar <* char ']')

parseVarOrLabelName :: Parser Text
parseVarOrLabelName = do
  (t, _) <-
    match (letterChar >> takeWhileP Nothing (\c -> isAlphaNum c || c == '_'))
  pure t

-- do

--  first <- letterChar
--  rest <- many (alphaNumChar <|> char '_')
--  return (first : rest)

-- Parser for operands
parseOperand :: Parser Operand
parseOperand =
  label "parsingOperand" $
    choice
      [ RegOp <$> try parseRegister
      , ImmOp
          <$> (try (Right <$> parseImmediate16) <|> try (Left <$> parseImmediate8))
      , MemOp <$> try parseMemory
      , do
          c <- lookAhead anySingle
          failure (Just $ Label (c :| [])) mempty
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
          <$> (L.symbol' hspace1 "int" *> parseImmediate8)
      , JNS <$> (L.symbol' hspace1 "jns" *> parseVarOrLabelName)
      , JMP <$> (L.symbol' hspace1 "jmp" *> parseVarOrLabelName)
      , INC <$> (L.symbol' hspace1 "inc" *> parseOperand)
      , binaryP CMP "cmp"
      , JE <$> (L.symbol' hspace1 "je" *> parseVarOrLabelName)
      , RET <$ L.symbol' hspace "ret"
      ]
  where
    binaryP constr txt =
      constr
        <$> (L.symbol' hspace1 txt *> L.lexeme hspace parseOperand)
        <*> (char ',' *> hspace *> parseOperand)

parseDirective :: Parser Directive
parseDirective =
  choice
    [ ORG <$> (L.symbol' hspace1 "org" >> parseImmediate12)
    , END <$ L.symbol' hspace1 "end"
    , NAME
        <$> ( L.symbol' hspace1 "name" *> between "\"" "\"" (takeWhileP Nothing isAlphaNum)
            )
    , parseDBdirective
    ]
  where
    parseDBdirective =
      choice
        [ do
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

parseLabel :: Parser Text
parseLabel = L.lexeme hspace parseVarOrLabelName <* char ':'

-- Parser for a full line (instruction + optional label)
parseStatement :: Parser Statement
parseStatement =
  L.lexeme sc $
    choice
      [ try (Dir <$> parseDirective)
      , try (Lab <$> parseLabel)
      , try (Ins <$> parseInstruction)
      ]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

-- Main parser function
parseAssembly
  :: Text
  -> Either
      (ParseErrorBundle Text Void)
      [Statement]
parseAssembly =
  parse
    (sc *> some parseStatement <* eof)
    "myfile"

mainLocal :: Text -> IO (Maybe [Statement])
mainLocal assemblyCode = do
  case parseAssembly assemblyCode of
    Left err -> do
      putStrLn "Error: "
      putStrLn $ errorBundlePretty err
      pure Nothing
    Right statements -> do
      -- traverse_ print statements
      pure $ Just statements
