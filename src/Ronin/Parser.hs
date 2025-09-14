{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Ronin.Parser
  ( parseModule
  , parseExpression
  , parseType
  , parseDeclaration
  , parsePattern
  , parseSiegeExpression
  , parseError
  ) where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Pos

import Ronin.AST
import Ronin.Lexer

-- | Parse a complete module
parseModule :: Parser Module
parseModule = do
  pos <- getPosition
  skipMany (token (== Whitespace) <|> token (== Newline))
  
  -- Parse module declaration
  modName <- parseModuleName
  
  -- Parse imports
  imports <- many parseImport
  
  -- Parse declarations
  decls <- many parseDeclaration
  
  return $ Module modName imports decls (sourcePosToPosition pos)

-- | Parse module name
parseModuleName :: Parser Text
parseModuleName = do
  token (== KWModule)
  name <- token isIdent
  token (== KWWhere)
  return $ tokenText name
  where
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse import declaration
parseImport :: Parser Import
parseImport = do
  pos <- getPosition
  token (== KWImport)
  modName <- token isIdent
  alias <- optional (do
    token (== Ident "as")
    token isIdent)
  items <- optional (do
    token (== LParen)
    items <- sepBy parseImportItem (token (== Comma))
    token (== RParen)
    return items)
  return $ Import (tokenText modName) (fmap tokenText alias) items (sourcePosToPosition pos)
  where
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse import item
parseImportItem :: Parser ImportItem
parseImportItem = do
  name <- token isIdent
  alias <- optional (do
    token (== Ident "as")
    token isIdent)
  return $ ImportItem (tokenText name) (fmap tokenText alias)
  where
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse declaration
parseDeclaration :: Parser Declaration
parseDeclaration = do
  pos <- getPosition
  choice
    [ parseFunctionDecl
    , parseTypeDecl
    , parseDataDecl
    , parseModuleDecl
    ]
  where
    parseFunctionDecl = do
      name <- token isIdent
      patterns <- many parsePattern
      token (== OpEqual)
      expr <- parseExpression
      typ <- optional (do
        token (== Colon)
        parseType)
      return $ FunctionDecl (tokenText name) patterns expr (maybe (TypeVar "a" (sourcePosToPosition pos)) id typ) (sourcePosToPosition pos)
    
    parseTypeDecl = do
      token (== KWType)
      name <- token isTypeIdent
      vars <- many parseTypeVariable
      token (== OpEqual)
      typ <- parseType
      return $ TypeDecl (tokenText name) vars typ (sourcePosToPosition pos)
    
    parseDataDecl = do
      token (== KWData)
      name <- token isTypeIdent
      vars <- many parseTypeVariable
      token (== OpEqual)
      constructors <- sepBy parseConstructor (token (== OpPipe))
      return $ DataDecl (tokenText name) vars constructors (sourcePosToPosition pos)
    
    parseModuleDecl = do
      token (== KWModule)
      name <- token isIdent
      token (== KWWhere)
      decls <- many parseDeclaration
      return $ ModuleDecl (tokenText name) decls (sourcePosToPosition pos)
    
    isIdent (Ident _) = True
    isIdent _ = False
    
    isTypeIdent (TypeIdent _) = True
    isTypeIdent _ = False

-- | Parse constructor
parseConstructor :: Parser TypeConstructor
parseConstructor = do
  pos <- getPosition
  name <- token isTypeIdent
  args <- many parseType
  return $ TypeConstructor (tokenText name) args (sourcePosToPosition pos)
  where
    isTypeIdent (TypeIdent _) = True
    isTypeIdent _ = False

-- | Parse type variable
parseTypeVariable :: Parser TypeVariable
parseTypeVariable = do
  pos <- getPosition
  name <- token isIdent
  kind <- optional (do
    token (== Colon)
    parseType)
  return $ TypeVariable (tokenText name) kind (sourcePosToPosition pos)
  where
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse expression
parseExpression :: Parser Expression
parseExpression = parseApplication

-- | Parse function application
parseApplication :: Parser Expression
parseApplication = do
  expr <- parsePrimary
  args <- many parsePrimary
  pos <- getPosition
  return $ foldl (\e a -> App e a (sourcePosToPosition pos)) expr args

-- | Parse primary expressions
parsePrimary :: Parser Expression
parsePrimary = do
  pos <- getPosition
  choice
    [ parseLiteral
    , parseVariable
    , parseLambda
    , parseLet
    , parseCase
    , parseIf
    , parseList
    , parseTuple
    , parseRecord
    , parseSiege
    , parseParen
    ]

-- | Parse literal
parseLiteral :: Parser Expression
parseLiteral = do
  pos <- getPosition
  lit <- choice
    [ token isIntLit >>= return . IntLit . fromInteger
    , token isFloatLit >>= return . FloatLit
    , token isCharLit >>= return . CharLit
    , token isStringLit >>= return . StringLit
    , token isBoolLit >>= return . BoolLit
    , token isGoldenRatio >> return GoldenRatioLit
    , token isPi >> return PiLit
    , token isTau >> return TauLit
    , token isEuler >> return EulerLit
    ]
  return $ Lit lit (sourcePosToPosition pos)
  where
    isIntLit (LitInt n) = Just n
    isIntLit _ = Nothing
    
    isFloatLit (LitFloat f) = Just f
    isFloatLit _ = Nothing
    
    isCharLit (LitChar c) = Just c
    isCharLit _ = Nothing
    
    isStringLit (LitString s) = Just s
    isStringLit _ = Nothing
    
    isBoolLit (Ident "True") = Just True
    isBoolLit (Ident "False") = Just False
    isBoolLit _ = Nothing
    
    isGoldenRatio GoldenRatio = True
    isGoldenRatio _ = False
    
    isPi Pi = True
    isPi _ = False
    
    isTau Tau = True
    isTau _ = False
    
    isEuler Euler = True
    isEuler _ = False

-- | Parse variable
parseVariable :: Parser Expression
parseVariable = do
  pos <- getPosition
  name <- token isIdent
  return $ Var (tokenText name) (sourcePosToPosition pos)
  where
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse lambda expression
parseLambda :: Parser Expression
parseLambda = do
  pos <- getPosition
  token (== OpBackslash)
  patterns <- many parsePattern
  token (== Arrow)
  expr <- parseExpression
  return $ Lambda patterns expr (sourcePosToPosition pos)

-- | Parse let expression
parseLet :: Parser Expression
parseLet = do
  pos <- getPosition
  token (== KWLet)
  decls <- many parseDeclaration
  token (== KWIn)
  expr <- parseExpression
  return $ Let decls expr (sourcePosToPosition pos)

-- | Parse case expression
parseCase :: Parser Expression
parseCase = do
  pos <- getPosition
  token (== KWCase)
  expr <- parseExpression
  token (== KWOf)
  branches <- sepBy parseBranch (token (== Semicolon))
  return $ Case expr branches (sourcePosToPosition pos)
  where
    parseBranch = do
      pattern <- parsePattern
      token (== Arrow)
      expr <- parseExpression
      return (pattern, expr)

-- | Parse if expression
parseIf :: Parser Expression
parseIf = do
  pos <- getPosition
  token (== KWIf)
  cond <- parseExpression
  token (== KWThen)
  thenExpr <- parseExpression
  token (== KWElse)
  elseExpr <- parseExpression
  return $ If cond thenExpr elseExpr (sourcePosToPosition pos)

-- | Parse list
parseList :: Parser Expression
parseList = do
  pos <- getPosition
  token (== LBracket)
  exprs <- sepBy parseExpression (token (== Comma))
  token (== RBracket)
  return $ List exprs (sourcePosToPosition pos)

-- | Parse tuple
parseTuple :: Parser Expression
parseTuple = do
  pos <- getPosition
  token (== LParen)
  exprs <- sepBy1 parseExpression (token (== Comma))
  token (== RParen)
  return $ if length exprs == 1 then head exprs else Tuple exprs (sourcePosToPosition pos)

-- | Parse record
parseRecord :: Parser Expression
parseRecord = do
  pos <- getPosition
  token (== LBrace)
  fields <- sepBy parseField (token (== Comma))
  token (== RBrace)
  return $ Record fields (sourcePosToPosition pos)
  where
    parseField = do
      name <- token isIdent
      token (== OpEqual)
      expr <- parseExpression
      return (tokenText name, expr)
    
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse SIEGE expression
parseSiege :: Parser Expression
parseSiege = do
  pos <- getPosition
  token (== KWDo)
  siegeExpr <- parseSiegeExpression
  return $ Siege siegeExpr (sourcePosToPosition pos)

-- | Parse SIEGE expression
parseSiegeExpression :: Parser SiegeExpression
parseSiegeExpression = parseSiegeApplication

-- | Parse SIEGE application
parseSiegeApplication :: Parser SiegeExpression
parseSiegeApplication = do
  expr <- parseSiegePrimary
  args <- many parseSiegePrimary
  return $ foldl SiegeApp expr args

-- | Parse SIEGE primary expressions
parseSiegePrimary :: Parser SiegeExpression
parseSiegePrimary = do
  choice
    [ parseSiegeConst
    , parseSiegeTime
    , parseSiegeSin
    , parseSiegeCos
    , parseSiegeParen
    ]

-- | Parse SIEGE constant
parseSiegeConst :: Parser SiegeExpression
parseSiegeConst = do
  val <- token isFloatLit
  return $ SiegeConst (fromMaybe 0.0 val)
  where
    isFloatLit (LitFloat f) = Just f
    isFloatLit _ = Nothing

-- | Parse SIEGE time
parseSiegeTime :: Parser SiegeExpression
parseSiegeTime = do
  token (== Ident "time")
  return SiegeTime

-- | Parse SIEGE sine
parseSiegeSin :: Parser SiegeExpression
parseSiegeSin = do
  token (== Ident "sin")
  token (== LParen)
  expr <- parseSiegeExpression
  token (== RParen)
  return $ SiegeSin expr

-- | Parse SIEGE cosine
parseSiegeCos :: Parser SiegeExpression
parseSiegeCos = do
  token (== Ident "cos")
  token (== LParen)
  expr <- parseSiegeExpression
  token (== RParen)
  return $ SiegeCos expr

-- | Parse SIEGE parentheses
parseSiegeParen :: Parser SiegeExpression
parseSiegeParen = do
  token (== LParen)
  expr <- parseSiegeExpression
  token (== RParen)
  return expr

-- | Parse parentheses
parseParen :: Parser Expression
parseParen = do
  token (== LParen)
  expr <- parseExpression
  token (== RParen)
  return expr

-- | Parse pattern
parsePattern :: Parser Pattern
parsePattern = do
  pos <- getPosition
  choice
    [ parseVarPattern
    , parseLitPattern
    , parseConPattern
    , parseWildcardPattern
    , parseListPattern
    , parseTuplePattern
    , parseRecordPattern
    , parseAsPattern
    , parsePatternParen
    ]

-- | Parse variable pattern
parseVarPattern :: Parser Pattern
parseVarPattern = do
  pos <- getPosition
  name <- token isIdent
  return $ VarPattern (tokenText name) (sourcePosToPosition pos)
  where
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse literal pattern
parseLitPattern :: Parser Pattern
parseLitPattern = do
  pos <- getPosition
  lit <- parseLiteral
  return $ LitPattern (case lit of Lit l _ -> l) (sourcePosToPosition pos)

-- | Parse constructor pattern
parseConPattern :: Parser Pattern
parseConPattern = do
  pos <- getPosition
  name <- token isTypeIdent
  args <- many parsePattern
  return $ ConPattern (tokenText name) args (sourcePosToPosition pos)
  where
    isTypeIdent (TypeIdent _) = True
    isTypeIdent _ = False

-- | Parse wildcard pattern
parseWildcardPattern :: Parser Pattern
parseWildcardPattern = do
  pos <- getPosition
  token (== Ident "_")
  return $ WildcardPattern (sourcePosToPosition pos)

-- | Parse list pattern
parseListPattern :: Parser Pattern
parseListPattern = do
  pos <- getPosition
  token (== LBracket)
  patterns <- sepBy parsePattern (token (== Comma))
  token (== RBracket)
  return $ ListPattern patterns (sourcePosToPosition pos)

-- | Parse tuple pattern
parseTuplePattern :: Parser Pattern
parseTuplePattern = do
  pos <- getPosition
  token (== LParen)
  patterns <- sepBy1 parsePattern (token (== Comma))
  token (== RParen)
  return $ if length patterns == 1 then head patterns else TuplePattern patterns (sourcePosToPosition pos)

-- | Parse record pattern
parseRecordPattern :: Parser Pattern
parseRecordPattern = do
  pos <- getPosition
  token (== LBrace)
  fields <- sepBy parsePatternField (token (== Comma))
  token (== RBrace)
  return $ RecordPattern fields (sourcePosToPosition pos)
  where
    parsePatternField = do
      name <- token isIdent
      token (== OpEqual)
      pattern <- parsePattern
      return (tokenText name, pattern)
    
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse as pattern
parseAsPattern :: Parser Pattern
parseAsPattern = do
  pos <- getPosition
  pattern <- parsePattern
  token (== Ident "as")
  name <- token isIdent
  return $ AsPattern pattern (tokenText name) (sourcePosToPosition pos)
  where
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse pattern parentheses
parsePatternParen :: Parser Pattern
parsePatternParen = do
  token (== LParen)
  pattern <- parsePattern
  token (== RParen)
  return pattern

-- | Parse type
parseType :: Parser Type
parseType = parseFunctionType

-- | Parse function type
parseFunctionType :: Parser Type
parseFunctionType = do
  arg <- parseApplicationType
  choice
    [ do
        token (== Arrow)
        res <- parseFunctionType
        pos <- getPosition
        return $ TypeFun arg res (sourcePosToPosition pos)
    , return arg
    ]

-- | Parse application type
parseApplicationType :: Parser Type
parseApplicationType = do
  typ <- parsePrimaryType
  args <- many parsePrimaryType
  pos <- getPosition
  return $ foldl (\t a -> TypeCon (typeName t) [t, a] (sourcePosToPosition pos)) typ args
  where
    typeName (TypeCon name _ _) = name
    typeName (TypeVar name _) = name
    typeName _ = ""

-- | Parse primary type
parsePrimaryType :: Parser Type
parsePrimaryType = do
  pos <- getPosition
  choice
    [ parseTypeVar
    , parseTypeCon
    , parseTypeTuple
    , parseTypeList
    , parseTypeRecord
    , parseTypeParen
    ]

-- | Parse type variable
parseTypeVar :: Parser Type
parseTypeVar = do
  pos <- getPosition
  name <- token isIdent
  return $ TypeVar (tokenText name) (sourcePosToPosition pos)
  where
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse type constructor
parseTypeCon :: Parser Type
parseTypeCon = do
  pos <- getPosition
  name <- token isTypeIdent
  args <- many parsePrimaryType
  return $ TypeCon (tokenText name) args (sourcePosToPosition pos)
  where
    isTypeIdent (TypeIdent _) = True
    isTypeIdent _ = False

-- | Parse tuple type
parseTypeTuple :: Parser Type
parseTypeTuple = do
  pos <- getPosition
  token (== LParen)
  types <- sepBy1 parseType (token (== Comma))
  token (== RParen)
  return $ if length types == 1 then head types else TypeTuple types (sourcePosToPosition pos)

-- | Parse list type
parseTypeList :: Parser Type
parseTypeList = do
  pos <- getPosition
  token (== LBracket)
  typ <- parseType
  token (== RBracket)
  return $ TypeList typ (sourcePosToPosition pos)

-- | Parse record type
parseTypeRecord :: Parser Type
parseTypeRecord = do
  pos <- getPosition
  token (== LBrace)
  fields <- sepBy parseTypeField (token (== Comma))
  token (== RBrace)
  return $ TypeRecord fields (sourcePosToPosition pos)
  where
    parseTypeField = do
      name <- token isIdent
      token (== Colon)
      typ <- parseType
      return (tokenText name, typ)
    
    isIdent (Ident _) = True
    isIdent _ = False

-- | Parse type parentheses
parseTypeParen :: Parser Type
parseTypeParen = do
  token (== LParen)
  typ <- parseType
  token (== RParen)
  return typ

-- | Convert SourcePos to Position
sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition pos = Position
  { line = sourceLine pos
  , column = sourceColumn pos
  , sourceFile = Just $ sourceName pos
  }

-- | Parse error helper
parseError :: ParseError -> String
parseError err = "Parse error at " ++ show (errorPos err) ++ ": " ++ show err

-- | Helper to check if token matches condition
token :: (TokenType -> Bool) -> Parser Token
token pred = do
  tok <- anyToken
  if pred (tokenType tok)
    then return tok
    else fail $ "Expected token matching predicate, got " ++ show (tokenType tok)
