{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Ronin.AST
  ( -- AST Data Types
    Module(..)
  , Declaration(..)
  , Expression(..)
  , Pattern(..)
  , Type(..)
  , TypeConstructor(..)
  , TypeVariable(..)
  , Literal(..)
  , Operator(..)
  , -- Position Information
    Position(..)
  , Located(..)
  , -- SIEGE Integration
    SiegeExpression(..)
  , -- Helper Functions
    moduleName
  , declarationName
  , expressionType
  , typeVariables
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)
import GHC.Generics

-- | Position information for AST nodes
data Position = Position
  { line :: Int
  , column :: Int
  , sourceFile :: Maybe FilePath
  } deriving (Show, Eq, Ord, Generic)

-- | Located AST node with position information
data Located a = Located
  { node :: a
  , position :: Position
  } deriving (Show, Eq, Ord, Generic)

-- | RONIN Module
data Module = Module
  { modName :: Text
  , modImports :: [Import]
  , modDeclarations :: [Declaration]
  , modPosition :: Position
  } deriving (Show, Eq, Generic)

-- | Import declaration
data Import = Import
  { importModule :: Text
  , importAlias :: Maybe Text
  , importItems :: Maybe [ImportItem]
  , importPosition :: Position
  } deriving (Show, Eq, Generic)

-- | Import item
data ImportItem = ImportItem
  { importItemName :: Text
  , importItemAlias :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Top-level declarations
data Declaration
  = -- Function declaration
    FunctionDecl Text [Pattern] Expression Type Position
  | -- Type declaration
    TypeDecl Text [TypeVariable] Type Position
  | -- Data type declaration
    DataDecl Text [TypeVariable] [TypeConstructor] Position
  | -- Module declaration
    ModuleDecl Text [Declaration] Position
  deriving (Show, Eq, Generic)

-- | Expressions
data Expression
  = -- Literals
    Lit Literal Position
  | -- Variables
    Var Text Position
  | -- Function application
    App Expression Expression Position
  | -- Lambda abstraction
    Lambda [Pattern] Expression Position
  | -- Let binding
    Let [Declaration] Expression Position
  | -- Case expression
    Case Expression [(Pattern, Expression)] Position
  | -- If expression
    If Expression Expression Expression Position
  | -- Operator application
    Op Operator Expression Expression Position
  | -- List expression
    List [Expression] Position
  | -- Tuple expression
    Tuple [Expression] Position
  | -- Record construction
    Record [(Text, Expression)] Position
  | -- Record access
    RecordAccess Expression Text Position
  | -- SIEGE expression
    Siege SiegeExpression Position
  | -- Type annotation
    Annotated Expression Type Position
  deriving (Show, Eq, Generic)

-- | Patterns for pattern matching
data Pattern
  = -- Variable pattern
    VarPattern Text Position
  | -- Literal pattern
    LitPattern Literal Position
  | -- Constructor pattern
    ConPattern Text [Pattern] Position
  | -- Wildcard pattern
    WildcardPattern Position
  | -- List pattern
    ListPattern [Pattern] Position
  | -- Tuple pattern
    TuplePattern [Pattern] Position
  | -- Record pattern
    RecordPattern [(Text, Pattern)] Position
  | -- As pattern (pattern as variable)
    AsPattern Pattern Text Position
  deriving (Show, Eq, Generic)

-- | Types
data Type
  = -- Type variable
    TypeVar Text Position
  | -- Type constructor
    TypeCon Text [Type] Position
  | -- Function type
    TypeFun Type Type Position
  | -- Tuple type
    TypeTuple [Type] Position
  | -- List type
    TypeList Type Position
  | -- Record type
    TypeRecord [(Text, Type)] Position
  deriving (Show, Eq, Generic)

-- | Type constructors
data TypeConstructor = TypeConstructor
  { conName :: Text
  , conArgs :: [Type]
  , conPosition :: Position
  } deriving (Show, Eq, Generic)

-- | Type variables
data TypeVariable = TypeVariable
  { tvName :: Text
  , tvKind :: Maybe Type  -- Higher-kinded types
  , tvPosition :: Position
  } deriving (Show, Eq, Generic)

-- | Literals
data Literal
  = -- Integer literal
    IntLit Integer
  | -- Float literal
    FloatLit Double
  | -- Character literal
    CharLit Char
  | -- String literal
    StringLit Text
  | -- Boolean literal
    BoolLit Bool
  | -- Golden ratio constant
    GoldenRatioLit
  | -- Pi constant
    PiLit
  | -- Tau constant
    TauLit
  | -- Euler's number
    EulerLit
  deriving (Show, Eq, Generic)

-- | Operators
data Operator
  = -- Arithmetic
    OpPlus | OpMinus | OpTimes | OpDivide | OpPower | OpMod
  | -- Comparison
    OpEqual | OpNotEqual | OpLess | OpGreater | OpLessEqual | OpGreaterEqual
  | -- Logical
    OpAnd | OpOr | OpNot
  | -- Composition
    OpPipe | OpCompose | OpApply
  | -- Audio-specific
    OpMix | OpGain | OpFilter | OpDelay | OpReverb
  deriving (Show, Eq, Generic)

-- | SIEGE expressions for mathematical audio processing
data SiegeExpression
  = -- Basic SIEGE primitives
    SiegeConst Double
  | SiegeTime
  | SiegeSin SiegeExpression
  | SiegeCos SiegeExpression
  | SiegeAdd SiegeExpression SiegeExpression
  | SiegeMul SiegeExpression SiegeExpression
  | SiegeDiv SiegeExpression SiegeExpression
  | SiegePow SiegeExpression SiegeExpression
  | -- Advanced SIEGE constructs
    SiegeIntegral SiegeExpression
  | SiegeDerivative SiegeExpression
  | SiegeDelay SiegeExpression Double
  | SiegeFilter SiegeExpression Double Double  -- cutoff, resonance
  | SiegeEnvelope SiegeExpression Double Double Double Double  -- ADSR
  | -- Golden ratio integration
    SiegeGolden SiegeExpression
  | -- Complex mathematical functions
    SiegeExp SiegeExpression
  | SiegeLog SiegeExpression
  | SiegeSqrt SiegeExpression
  | -- Trigonometric functions
    SiegeTan SiegeExpression
  | SiegeAsin SiegeExpression
  | SiegeAcos SiegeExpression
  | SiegeAtan SiegeExpression
  | -- Special functions
    SiegeAbs SiegeExpression
  | SiegeSign SiegeExpression
  | SiegeMin SiegeExpression SiegeExpression
  | SiegeMax SiegeExpression SiegeExpression
  deriving (Show, Eq, Generic)

-- | Helper functions for AST manipulation

-- | Extract module name
moduleName :: Module -> Text
moduleName = modName

-- | Extract declaration name
declarationName :: Declaration -> Maybe Text
declarationName (FunctionDecl name _ _ _ _) = Just name
declarationName (TypeDecl name _ _ _) = Just name
declarationName (DataDecl name _ _ _) = Just name
declarationName (ModuleDecl name _ _) = Just name

-- | Extract expression type (if annotated)
expressionType :: Expression -> Maybe Type
expressionType (Annotated _ typ _) = Just typ
expressionType _ = Nothing

-- | Extract all type variables from a type
typeVariables :: Type -> [Text]
typeVariables (TypeVar name _) = [name]
typeVariables (TypeCon _ args _) = concatMap typeVariables args
typeVariables (TypeFun arg res _) = typeVariables arg ++ typeVariables res
typeVariables (TypeTuple types _) = concatMap typeVariables types
typeVariables (TypeList typ _) = typeVariables typ
typeVariables (TypeRecord fields _) = concatMap (typeVariables . snd) fields

-- | Remove duplicates from type variables
uniqueTypeVariables :: Type -> [Text]
uniqueTypeVariables = nub . typeVariables

-- | Check if a type is a function type
isFunctionType :: Type -> Bool
isFunctionType (TypeFun _ _ _) = True
isFunctionType _ = False

-- | Get function argument and result types
functionTypeArgs :: Type -> Maybe (Type, Type)
functionTypeArgs (TypeFun arg res _) = Just (arg, res)
functionTypeArgs _ = Nothing

-- | Check if a pattern is irrefutable (always matches)
isIrrefutable :: Pattern -> Bool
isIrrefutable (VarPattern _ _) = True
isIrrefutable (WildcardPattern _) = True
isIrrefutable (AsPattern _ _ _) = True
isIrrefutable (RecordPattern fields _) = all (isIrrefutable . snd) fields
isIrrefutable _ = False

-- | Extract variables bound by a pattern
patternVariables :: Pattern -> [Text]
patternVariables (VarPattern name _) = [name]
patternVariables (AsPattern pat name _) = name : patternVariables pat
patternVariables (ConPattern _ args _) = concatMap patternVariables args
patternVariables (ListPattern pats _) = concatMap patternVariables pats
patternVariables (TuplePattern pats _) = concatMap patternVariables pats
patternVariables (RecordPattern fields _) = concatMap (patternVariables . snd) fields
patternVariables _ = []

-- | Check if SIEGE expression is constant
isConstantSiege :: SiegeExpression -> Bool
isConstantSiege (SiegeConst _) = True
isConstantSiege (SiegeTime) = False
isConstantSiege (SiegeSin expr) = isConstantSiege expr
isConstantSiege (SiegeCos expr) = isConstantSiege expr
isConstantSiege (SiegeAdd e1 e2) = isConstantSiege e1 && isConstantSiege e2
isConstantSiege (SiegeMul e1 e2) = isConstantSiege e1 && isConstantSiege e2
isConstantSiege (SiegeDiv e1 e2) = isConstantSiege e1 && isConstantSiege e2
isConstantSiege (SiegePow e1 e2) = isConstantSiege e1 && isConstantSiege e2
isConstantSiege (SiegeIntegral expr) = isConstantSiege expr
isConstantSiege (SiegeDerivative expr) = isConstantSiege expr
isConstantSiege (SiegeDelay expr _) = isConstantSiege expr
isConstantSiege (SiegeFilter expr _ _) = isConstantSiege expr
isConstantSiege (SiegeEnvelope expr _ _ _ _) = isConstantSiege expr
isConstantSiege (SiegeGolden expr) = isConstantSiege expr
isConstantSiege (SiegeExp expr) = isConstantSiege expr
isConstantSiege (SiegeLog expr) = isConstantSiege expr
isConstantSiege (SiegeSqrt expr) = isConstantSiege expr
isConstantSiege (SiegeTan expr) = isConstantSiege expr
isConstantSiege (SiegeAsin expr) = isConstantSiege expr
isConstantSiege (SiegeAcos expr) = isConstantSiege expr
isConstantSiege (SiegeAtan expr) = isConstantSiege expr
isConstantSiege (SiegeAbs expr) = isConstantSiege expr
isConstantSiege (SiegeSign expr) = isConstantSiege expr
isConstantSiege (SiegeMin e1 e2) = isConstantSiege e1 && isConstantSiege e2
isConstantSiege (SiegeMax e1 e2) = isConstantSiege e1 && isConstantSiege e2
