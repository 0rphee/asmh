{-# LANGUAGE LiberalTypeSynonyms #-}

module Parser where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Int
import Data.List.NonEmpty qualified as N
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (take)

type Parser = Parsec Void Text

-- Parser for registers
parseRegister :: Parser (SomeSized Register)
parseRegister =
  label "register" $
    choice
      [ SZ16 AX <$ string' "AX"
      , SZ16 BX <$ string' "BX"
      , SZ16 CX <$ string' "CX"
      , SZ16 DX <$ string' "DX"
      , SZ16 SI <$ string' "SI"
      , SZ16 DI <$ string' "DI"
      , SZ16 BP <$ string' "BP"
      , SZ16 SP <$ string' "SP"
      , SZ8 AL <$ string' "AL"
      , SZ8 BL <$ string' "BL"
      , SZ8 CL <$ string' "CL"
      , SZ8 DL <$ string' "DL"
      , SZ8 AH <$ string' "AH"
      , SZ8 BH <$ string' "BH"
      , SZ8 CH <$ string' "CH"
      , SZ8 DH <$ string' "DH"
      ]

parseNum :: Num b => Parser b -> Parser a -> String -> Parser b
parseNum lexer ending lstr = label lstr $ L.signed hspace $ do
  o <- getOffset
  region (\_ -> TrivialError o Nothing $ S.singleton $ Label $ N.fromList lstr) $ do
    val <- lexer
    ending
    pure val

parseBin :: Parser Int
parseBin = parseNum L.binary (char 'b') "binary number"

parseHex :: Parser Int
parseHex = parseNum L.hexadecimal (char 'h') "hexadecimal number"

parseChar :: Parser Int
parseChar =
  label "character enclosed in <'>" $
    ord <$> between "'" "'" anySingle

parseDecimal :: Parser Int
parseDecimal =
  label "decimal number" $
    L.lexeme hspace $
      L.signed
        hspace
        (L.decimal <* lookAhead (label "no postfix for number" spaceChar))

parseImmediate :: Parser (SomeSized RawValue)
parseImmediate =
  label
    "immediate value"
    ( SZS . RWS
        <$> choice
          [ try parseHex
          , try parseBin
          , try parseChar
          , parseDecimal
          ]
    )

warnIfOverUnderflow
  :: forall a. (Bounded a, Integral a) => Parser Int -> Parser a
warnIfOverUnderflow p = do
  o <- getOffset
  i <- p
  when (i > fromIntegral (maxBound @a)) $
    registerParseError $
      TrivialError o (Just $ Label $ N.fromList "overfloow") S.empty
  when (i < fromIntegral (minBound @a)) $
    registerParseError $
      TrivialError o (Just $ Label $ N.fromList "underfloow") S.empty
  pure $ fromIntegral i

-- Parser for memory operands (simplified, just accepting labels for now)
parseMemory :: Parser Text
parseMemory = label "memory address" $ T.pack <$> (char '[' *> some letterChar <* char ']')

parseVarOrLabelName :: Parser Text
parseVarOrLabelName = do
  (t, _) <-
    match (letterChar >> takeWhileP Nothing (\c -> isAlphaNum c || c == '_'))
  pure t

type OpPair (s :: Size) = (Operand s, Operand s)

-- Parser for operands
parseOperand :: Maybe (SomeSized Operand) -> Parser (SomeSized OpPair)
parseOperand op = label "Instruction operand" $ do
  partialres <-
    choice
      [ try $ rewrapSized RegOp <$> parseRegister
      , try $ rewrapSized ImmOp <$> parseImmediate
      , SZS . MemOp <$> parseMemory
      ]
  case op of
    Nothing -> pure partialres
    Just o -> do
      case (o, partialres) of
        (SZ8 _, SZ8 _) -> pure partialres
        (SZ16 _, SZ16 _) -> pure partialres
        (SZS _, _) -> pure partialres
        _ -> fail " "

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
        , singleP INT "int" (warnIfOverUnderflow parseImmediate)
        , singleP JNS "jns" parseVarOrLabelName
        , singleP JMP "jmp" parseVarOrLabelName
        , singleP JE "je" parseVarOrLabelName
        , singleP INC "inc" parseOperand
        , binaryP CMP "cmp"
        , RET <$ L.symbol' hspace "ret"
        ]
  where
    singleP constr txt arg =
      constr <$> do
        L.symbol' hspace1 txt *> arg
    binaryP constr txt = do
      L.symbol' hspace1 txt
      op1 <- label "op1" (L.lexeme hspace (parseOperand Nothing))
      op2 <-
        label
          "op2"
          (char ',' *> hidden hspace *> L.lexeme hspace (parseOperand (Just op1)))
      pure $ constr

parseDirective :: Parser Directive
parseDirective =
  label "Compiler directive" $
    choice
      [ ORG <$> (L.symbol' hspace1 "org" >> warnIfOverUnderflow parseImmediate)
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
              label "Variable name" $
                T.toLower
                  <$> L.lexeme hspace (takeWhile1P (Just "variable char") isLetter)

            L.symbol' hspace1 "db"
            v <- sepBy1 (L.lexeme hspace (warnIfOverUnderflow parseImmediate)) (char ',')
            when (vname == "db") $ fail "you cannot name a variable 'db'"
            pure $ DB $ Left (vname, v)
        , do
            L.symbol' hspace1 "db"
            v <- sepBy1 (L.lexeme hspace (warnIfOverUnderflow parseImmediate)) (char ',')
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
parseTestHelper assemblyCode = case parseAssembly "test" assemblyCode of
  Left err -> do
    putStrLn "Error: "
    putStrLn $ errorBundlePretty err
    pure Nothing
  Right statements -> do
    -- traveabse_ print statements
    pure $ Just statements

parseTest :: IO (Maybe [Statement])
parseTest = parseTestHelper "mov ah, \"text3243242moremore\""

-- mainLocal "mov ah, 3243242moremore\""