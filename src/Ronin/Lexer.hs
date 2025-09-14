{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Ronin.Lexer
  ( Token(..)
  , TokenType(..)
  , lexer
  , tokenize
  ) where

import Control.Monad.State
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

-- | Token types in the RONIN language
data TokenType
  = -- Keywords
    KWModule | KWWhere | KWData | KWType | KWImport | KWLet | KWIn
  | KWIf | KWThen | KWElse | KWCase | KWOf | KWDo | KWRec
  | -- Operators
    OpPlus | OpMinus | OpTimes | OpDivide | OpPower | OpMod
  | OpEqual | OpNotEqual | OpLess | OpGreater | OpLessEqual | OpGreaterEqual
  | OpAnd | OpOr | OpNot | OpPipe | OpCompose | OpApply
  | -- Delimiters
    LParen | RParen | LBrace | RBrace | LBracket | RBracket
  | Semicolon | Comma | Colon | Arrow | Dot | Pipe
  | -- Literals
    LitInt Integer | LitFloat Double | LitChar Char | LitString Text
  | -- Identifiers
    Ident Text | TypeIdent Text | ModuleIdent Text
  | -- Special symbols
    GoldenRatio | Pi | Tau | Euler
  | -- Comments and whitespace
    Comment Text | Newline | Whitespace
  | -- End of file
    EOF
  deriving (Show, Eq)

-- | Token with position information
data Token = Token
  { tokenType :: TokenType
  , tokenText :: Text
  , tokenPos :: SourcePos
  } deriving (Show, Eq)

-- | Golden ratio constant
goldenRatio :: Double
goldenRatio = (1 + sqrt 5) / 2

-- | Main lexer function
lexer :: Parser [Token]
lexer = many token <* eof

-- | Tokenize a single token
token :: Parser Token
token = do
  pos <- getPosition
  choice
    [ comment
    , whitespace
    , keyword
    , operator
    , delimiter
    , literal
    , identifier
    , specialSymbol
    ]

-- | Keywords
keyword :: Parser Token
keyword = do
  pos <- getPosition
  kw <- choice
    [ try (string "module" >> return KWModule)
    , try (string "where" >> return KWWhere)
    , try (string "data" >> return KWData)
    , try (string "type" >> return KWType)
    , try (string "import" >> return KWImport)
    , try (string "let" >> return KWLet)
    , try (string "in" >> return KWIn)
    , try (string "if" >> return KWIf)
    , try (string "then" >> return KWThen)
    , try (string "else" >> return KWElse)
    , try (string "case" >> return KWCase)
    , try (string "of" >> return KWOf)
    , try (string "do" >> return KWDo)
    , try (string "rec" >> return KWRec)
    ]
  return $ Token kw (T.pack $ show kw) pos

-- | Operators
operator :: Parser Token
operator = do
  pos <- getPosition
  op <- choice
    [ try (string "+" >> return OpPlus)
    , try (string "-" >> return OpMinus)
    , try (string "*" >> return OpTimes)
    , try (string "/" >> return OpDivide)
    , try (string "^" >> return OpPower)
    , try (string "%" >> return OpMod)
    , try (string "==" >> return OpEqual)
    , try (string "/=" >> return OpNotEqual)
    , try (string "<" >> return OpLess)
    , try (string ">" >> return OpGreater)
    , try (string "<=" >> return OpLessEqual)
    , try (string ">=" >> return OpGreaterEqual)
    , try (string "&&" >> return OpAnd)
    , try (string "||" >> return OpOr)
    , try (string "!" >> return OpNot)
    , try (string "|>" >> return OpPipe)
    , try (string ">>" >> return OpCompose)
    , try (string "$" >> return OpApply)
    ]
  return $ Token op (T.pack $ show op) pos

-- | Delimiters
delimiter :: Parser Token
delimiter = do
  pos <- getPosition
  delim <- choice
    [ char '(' >> return LParen
    , char ')' >> return RParen
    , char '{' >> return LBrace
    , char '}' >> return RBrace
    , char '[' >> return LBracket
    , char ']' >> return RBracket
    , char ';' >> return Semicolon
    , char ',' >> return Comma
    , char ':' >> return Colon
    , string "->" >> return Arrow
    , char '.' >> return Dot
    , char '|' >> return Pipe
    ]
  return $ Token delim (T.pack $ show delim) pos

-- | Literals
literal :: Parser Token
literal = do
  pos <- getPosition
  choice
    [ floatLiteral pos
    , intLiteral pos
    , charLiteral pos
    , stringLiteral pos
    ]

-- | Integer literal
intLiteral :: SourcePos -> Parser Token
intLiteral pos = do
  digits <- many1 digit
  return $ Token (LitInt $ read digits) (T.pack digits) pos

-- | Float literal
floatLiteral :: SourcePos -> Parser Token
floatLiteral pos = do
  digits <- many1 digit
  char '.'
  decimals <- many1 digit
  let floatStr = digits ++ "." ++ decimals
  return $ Token (LitFloat $ read floatStr) (T.pack floatStr) pos

-- | Character literal
charLiteral :: SourcePos -> Parser Token
charLiteral pos = do
  char '\''
  c <- anyChar
  char '\''
  return $ Token (LitChar c) (T.pack ['\'', c, '\'']) pos

-- | String literal
stringLiteral :: SourcePos -> Parser Token
stringLiteral pos = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  let strText = T.pack str
  return $ Token (LitString strText) (T.pack $ "\"" ++ str ++ "\"") pos

-- | Identifiers
identifier :: Parser Token
identifier = do
  pos <- getPosition
  first <- letter <|> char '_'
  rest <- many (letter <|> digit <|> char '_' <|> char '\'')
  let ident = T.pack (first : rest)
  
  -- Check if it's a type identifier (starts with capital)
  if isUpper first
    then return $ Token (TypeIdent ident) ident pos
    else return $ Token (Ident ident) ident pos

-- | Special mathematical symbols
specialSymbol :: Parser Token
specialSymbol = do
  pos <- getPosition
  sym <- choice
    [ try (string "φ" >> return GoldenRatio)
    , try (string "π" >> return Pi)
    , try (string "τ" >> return Tau)
    , try (string "e" >> return Euler)
    ]
  return $ Token sym (T.pack $ show sym) pos

-- | Comments
comment :: Parser Token
comment = do
  pos <- getPosition
  string "--"
  content <- many (noneOf "\n")
  return $ Token (Comment $ T.pack content) (T.pack $ "--" ++ content) pos

-- | Whitespace
whitespace :: Parser Token
whitespace = do
  pos <- getPosition
  ws <- many1 (char ' ' <|> char '\t')
  return $ Token Whitespace (T.pack ws) pos

-- | Newline
newline :: Parser Token
newline = do
  pos <- getPosition
  char '\n'
  return $ Token Newline "\n" pos

-- | Tokenize text input
tokenize :: Text -> Either ParseError [Token]
tokenize input = parse lexer "" input

-- | Helper function to check if character is valid in identifier
isValidIdentChar :: Char -> Bool
isValidIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | Get token text for display
tokenText :: Token -> Text
tokenText = tokenText
