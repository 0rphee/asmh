module Parser where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.List.NonEmpty qualified as N
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (take)

type Parser = Parsec Void Text

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

parseNum :: Parser b -> Parser a -> Int -> String -> Parser b
parseNum lexer ending numberOfDigits lstr = label lstr $ do
  o <- getOffset
  region (\_ -> TrivialError o Nothing $ S.singleton $ Label $ N.fromList lstr) $ do
    (toks, val) <- match lexer
    guard (T.length toks == numberOfDigits)
    ending
    pure val

parseBin :: Num b => Int -> Parser b
parseBin ndigits = parseNum L.binary (char 'b') ndigits lstr
  where
    lstr =
      concat
        [ "binary number of "
        , show ndigits
        , " digits ["
        , show $ ndigits `div` 8
        , " byte(s)]"
        ]

parseBin8 :: Parser Word8
parseBin8 = parseBin 8

parseBin12 :: Parser Word16
parseBin12 = parseBin 12

parseBin16 :: Parser Word16
parseBin16 = parseBin 16

parseHex :: Num b => Int -> Parser b
parseHex ndigits = parseNum L.hexadecimal (char 'h') ndigits lstr
  where
    lstr =
      concat
        [ "hexadecimal number of "
        , show ndigits
        , " digits ["
        , show $ ndigits `div` 2
        , " bytes(s)]"
        ]

parseHex8 :: Parser Word8
parseHex8 = parseHex 2

parseHex12 :: Parser Word16
parseHex12 = parseHex 3

parseHex16 :: Parser Word16
parseHex16 = parseHex 4

parseChar :: Parser Char
parseChar =
  label "character enclosed in <'>" $
    between "'" "'" anySingle

parseDecimal :: Parser Int
parseDecimal =
  label "decimal number" $
    L.lexeme space $
      hidden L.decimal <* lookAhead (label "no postfix for number" spaceChar)

-- Parser for immediate values
parseImmediate8 :: Parser Word8
parseImmediate8 =
  label "8bit number" $
    choice
      [ try parseBin8
      , parseHex8
      ]

parseImmediate12 :: Parser Word16
parseImmediate12 =
  label "12bit number" $
    choice
      [ try parseBin12
      , parseHex12
      ]

parseImmediate16 :: Parser Word16
parseImmediate16 =
  label "16bit number" $
    choice
      [ try parseBin16
      , parseHex16
      ]

-- Parser for memory operands (simplified, just accepting labels for now)
parseMemory :: Parser Text
parseMemory = T.pack <$> (char '[' *> some letterChar <* char ']')

parseVarOrLabelName :: Parser Text
parseVarOrLabelName = do
  (t, _) <-
    match (letterChar >> takeWhileP Nothing (\c -> isAlphaNum c || c == '_'))
  pure t

-- Parser for operands
parseOperand :: Parser Operand
parseOperand =
  -- label "Instruction operand" $
  choice
    [ try $ label "register" $ RegOp <$> parseRegister
    , label "immediate value" $
        ImmOp
          <$> choice
            [ try $ RInt <$> parseDecimal
            , try $ RW16 <$> parseImmediate16
            , try $ RW8 <$> parseImmediate8
            , RChar <$> parseChar
            ]
    , label "memory address" $ MemOp <$> parseMemory
    ]

-- Parser for instructions
parseInstruction :: Parser Instruction
parseInstruction =
  label "Instruction" $
    L.lexeme hspace $
      choice
        [ binaryP MOV "mov"
        , binaryP ADD "add"
        , binaryP SUB "sub"
        , binaryP OR "or"
        , INT <$> (L.symbol' hspace1 "int" *> parseImmediate8)
        , JNS <$> (L.symbol' hspace1 "jns" *> parseVarOrLabelName)
        , JMP <$> (L.symbol' hspace1 "jmp" *> parseVarOrLabelName)
        , INC <$> (L.symbol' hspace1 "inc" *> parseOperand)
        , binaryP CMP "cmp"
        , JE <$> (L.symbol' hspace1 "je" *> parseVarOrLabelName)
        , RET <$ L.symbol' hspace "ret"
        ]
  where
    binaryP constr txt = do
      L.symbol' hspace1 txt
      constr
        <$> label "op1" (L.lexeme hspace parseOperand)
        <*> label "op2" (char ',' *> hidden hspace *> L.lexeme hspace parseOperand)

parseDirective :: Parser Directive
parseDirective =
  label "Compiler directive" $
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
parseLabel = label "Label" $ L.lexeme hspace parseVarOrLabelName <* char ':'

-- Parser for a full line (instruction + optional label)
parseStatement :: Parser Statement
parseStatement =
  L.lexeme sc $
    choice
      [ try (Dir <$> parseDirective)
      , try (Lab <$> parseLabel)
      , Ins <$> parseInstruction
      ]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

-- Main parser function
parseAssembly
  :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Statement]
parseAssembly =
  parse
    (sc *> some parseStatement <* eof)

parseTestHelper :: Text -> IO (Maybe [Statement])
parseTestHelper assemblyCode = do
  case parseAssembly "test" assemblyCode of
    Left err -> do
      putStrLn "Error: "
      putStrLn $ errorBundlePretty err
      pure Nothing
    Right statements -> do
      -- traveabse_ print statements
      pure $ Just statements

parseTest :: IO (Maybe [Statement])
parseTest = do
  parseTestHelper "mov ah, \"text3243242moremore\""

-- mainLocal "mov ah, 3243242moremore\""