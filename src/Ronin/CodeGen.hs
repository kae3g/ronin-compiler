{-# LANGUAGE OverloadedStrings #-}
module Ronin.CodeGen
  ( CodeGenerator
  , CodeGenError(..)
  , Target(..)
  , generateCode
  , generateHaskell
  , generateRust
  , generateClojure
  ) where

import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint

import Ronin.AST

-- | Code generation targets
data Target
  = Haskell
  | Rust
  | Clojure
  | WebAssembly
  deriving (Show, Eq)

-- | Code generation errors
data CodeGenError
  = UnsupportedTarget Target
  | UnsupportedExpression Expression
  | UnsupportedType Type
  | CodeGenError Text
  deriving (Show, Eq)

-- | Code generation monad
type CodeGenerator = StateT CodeGenState (Except CodeGenError)

-- | Code generation state
data CodeGenState = CodeGenState
  { indentLevel :: Int
  , currentModule :: Maybe Text
  , imports :: [Text]
  , definitions :: [Text]
  } deriving (Show, Eq)

-- | Generate code for a module
generateCode :: Target -> Module -> Either CodeGenError Text
generateCode target module_ = 
  case target of
    Haskell -> generateHaskell module_
    Rust -> generateRust module_
    Clojure -> generateClojure module_
    WebAssembly -> Left $ UnsupportedTarget WebAssembly

-- | Generate Haskell code
generateHaskell :: Module -> Either CodeGenError Text
generateHaskell module_ = runExcept $ evalStateT (generateHaskellModule module_) emptyState

-- | Generate Haskell module
generateHaskellModule :: Module -> CodeGenerator Text
generateHaskellModule (Module name imports decls pos) = do
  -- Generate module header
  header <- generateModuleHeader name imports
  
  -- Generate declarations
  declsText <- mapM generateHaskellDeclaration decls
  
  -- Combine all parts
  return $ T.unlines $ header : concat declsText

-- | Generate module header
generateModuleHeader :: Text -> [Import] -> CodeGenerator [Text]
generateModuleHeader name imports = do
  let header = ["module " <> name <> " where", ""]
  importTexts <- mapM generateHaskellImport imports
  return $ header ++ importTexts ++ [""]

-- | Generate Haskell import
generateHaskellImport :: Import -> CodeGenerator Text
generateHaskellImport (Import moduleName alias items pos) = do
  let importLine = "import " <> moduleName
  case alias of
    Just aliasName -> return $ importLine <> " as " <> aliasName
    Nothing -> return importLine

-- | Generate Haskell declaration
generateHaskellDeclaration :: Declaration -> CodeGenerator [Text]
generateHaskellDeclaration (FunctionDecl name patterns expr typ pos) = do
  -- Generate function signature
  signature <- generateHaskellType typ
  let sigLine = name <> " :: " <> signature
  
  -- Generate function body
  body <- generateHaskellExpression expr
  
  return [sigLine, name <> " = " <> body, ""]

generateHaskellDeclaration (TypeDecl name vars typ pos) = do
  typText <- generateHaskellType typ
  let typeLine = "type " <> name <> " = " <> typText
  return [typeLine, ""]

generateHaskellDeclaration (DataDecl name vars constructors pos) = do
  let dataLine = "data " <> name <> " = " <> T.intercalate " | " (map conName constructors)
  return [dataLine, ""]

generateHaskellDeclaration (ModuleDecl name decls pos) = do
  -- Generate nested module declarations
  concat <$> mapM generateHaskellDeclaration decls

-- | Generate Haskell expression
generateHaskellExpression :: Expression -> CodeGenerator Text
generateHaskellExpression (Lit lit pos) = 
  return $ generateHaskellLiteral lit

generateHaskellExpression (Var name pos) = 
  return name

generateHaskellExpression (App func arg pos) = do
  funcText <- generateHaskellExpression func
  argText <- generateHaskellExpression arg
  return $ "(" <> funcText <> " " <> argText <> ")"

generateHaskellExpression (Lambda patterns expr pos) = do
  patternTexts <- mapM generateHaskellPattern patterns
  exprText <- generateHaskellExpression expr
  return $ "\\" <> T.intercalate " " patternTexts <> " -> " <> exprText

generateHaskellExpression (Siege siegeExpr pos) = do
  return $ "siege $ " <> generateHaskellSiegeExpression siegeExpr

generateHaskellExpression _ = 
  throwError $ CodeGenError "Unsupported expression in Haskell generation"

-- | Generate Haskell literal
generateHaskellLiteral :: Literal -> Text
generateHaskellLiteral (IntLit n) = T.pack $ show n
generateHaskellLiteral (FloatLit f) = T.pack $ show f
generateHaskellLiteral (CharLit c) = T.pack $ show c
generateHaskellLiteral (StringLit s) = T.pack $ show s
generateHaskellLiteral (BoolLit True) = "True"
generateHaskellLiteral (BoolLit False) = "False"
generateHaskellLiteral GoldenRatioLit = "goldenRatio"
generateHaskellLiteral PiLit = "pi"
generateHaskellLiteral TauLit = "tau"
generateHaskellLiteral EulerLit = "exp 1"

-- | Generate Haskell pattern
generateHaskellPattern :: Pattern -> CodeGenerator Text
generateHaskellPattern (VarPattern name pos) = return name
generateHaskellPattern (LitPattern lit pos) = return $ generateHaskellLiteral lit
generateHaskellPattern (WildcardPattern pos) = return "_"
generateHaskellPattern _ = throwError $ CodeGenError "Unsupported pattern in Haskell generation"

-- | Generate Haskell type
generateHaskellType :: Type -> CodeGenerator Text
generateHaskellType (TypeVar name pos) = return name
generateHaskellType (TypeCon name [] pos) = return name
generateHaskellType (TypeCon name args pos) = do
  argTexts <- mapM generateHaskellType args
  return $ name <> " " <> T.intercalate " " argTexts
generateHaskellType (TypeFun arg res pos) = do
  argText <- generateHaskellType arg
  resText <- generateHaskellType res
  return $ argText <> " -> " <> resText
generateHaskellType _ = throwError $ CodeGenError "Unsupported type in Haskell generation"

-- | Generate Haskell SIEGE expression
generateHaskellSiegeExpression :: SiegeExpression -> Text
generateHaskellSiegeExpression (SiegeConst val) = T.pack $ show val
generateHaskellSiegeExpression SiegeTime = "time"
generateHaskellSiegeExpression (SiegeSin expr) = "sin (" <> generateHaskellSiegeExpression expr <> ")"
generateHaskellSiegeExpression (SiegeCos expr) = "cos (" <> generateHaskellSiegeExpression expr <> ")"
generateHaskellSiegeExpression (SiegeAdd e1 e2) = 
  "(" <> generateHaskellSiegeExpression e1 <> " + " <> generateHaskellSiegeExpression e2 <> ")"
generateHaskellSiegeExpression (SiegeMul e1 e2) = 
  "(" <> generateHaskellSiegeExpression e1 <> " * " <> generateHaskellSiegeExpression e2 <> ")"

-- | Generate Rust code
generateRust :: Module -> Either CodeGenError Text
generateRust module_ = runExcept $ evalStateT (generateRustModule module_) emptyState

-- | Generate Rust module
generateRustModule :: Module -> CodeGenerator Text
generateRustModule (Module name imports decls pos) = do
  -- Generate Rust module
  header <- generateRustHeader
  declsText <- mapM generateRustDeclaration decls
  return $ T.unlines $ header : concat declsText

-- | Generate Rust header
generateRustHeader :: CodeGenerator [Text]
generateRustHeader = return
  [ "// Generated by RONIN Compiler"
  , "use std::f64;"
  , ""
  ]

-- | Generate Rust declaration
generateRustDeclaration :: Declaration -> CodeGenerator [Text]
generateRustDeclaration (FunctionDecl name patterns expr typ pos) = do
  exprText <- generateRustExpression expr
  return [ "pub fn " <> name <> "() -> f64 {" ]
         ++ [ "    " <> exprText ]
         ++ [ "}" ]
         ++ [ "" ]

generateRustDeclaration _ = 
  throwError $ CodeGenError "Unsupported declaration in Rust generation"

-- | Generate Rust expression
generateRustExpression :: Expression -> CodeGenerator Text
generateRustExpression (Lit lit pos) = 
  return $ generateRustLiteral lit

generateRustExpression (Siege siegeExpr pos) = 
  return $ generateRustSiegeExpression siegeExpr

generateRustExpression _ = 
  throwError $ CodeGenError "Unsupported expression in Rust generation"

-- | Generate Rust literal
generateRustLiteral :: Literal -> Text
generateRustLiteral (IntLit n) = T.pack $ show n <> ".0"
generateRustLiteral (FloatLit f) = T.pack $ show f
generateRustLiteral GoldenRatioLit = "1.618033988749895"
generateRustLiteral PiLit = "f64::consts::PI"
generateRustLiteral TauLit = "f64::consts::TAU"
generateRustLiteral EulerLit = "f64::consts::E"

-- | Generate Rust SIEGE expression
generateRustSiegeExpression :: SiegeExpression -> Text
generateRustSiegeExpression (SiegeConst val) = T.pack $ show val
generateRustSiegeExpression SiegeTime = "time"
generateRustSiegeExpression (SiegeSin expr) = "(" <> generateRustSiegeExpression expr <> ").sin()"

-- | Generate Clojure code
generateClojure :: Module -> Either CodeGenError Text
generateClojure module_ = runExcept $ evalStateT (generateClojureModule module_) emptyState

-- | Generate Clojure module
generateClojureModule :: Module -> CodeGenerator Text
generateClojureModule (Module name imports decls pos) = do
  header <- generateClojureHeader name
  declsText <- mapM generateClojureDeclaration decls
  return $ T.unlines $ header : concat declsText

-- | Generate Clojure header
generateClojureHeader :: Text -> CodeGenerator [Text]
generateClojureHeader name = return
  [ "(ns " <> name <> ")"
  , ""
  ]

-- | Generate Clojure declaration
generateClojureDeclaration :: Declaration -> CodeGenerator [Text]
generateClojureDeclaration (FunctionDecl name patterns expr typ pos) = do
  exprText <- generateClojureExpression expr
  return [ "(defn " <> name <> " []" ]
         ++ [ "  " <> exprText ]
         ++ [ ")" ]
         ++ [ "" ]

generateClojureDeclaration _ = 
  throwError $ CodeGenError "Unsupported declaration in Clojure generation"

-- | Generate Clojure expression
generateClojureExpression :: Expression -> CodeGenerator Text
generateClojureExpression (Lit lit pos) = 
  return $ generateClojureLiteral lit

generateClojureExpression (Siege siegeExpr pos) = 
  return $ generateClojureSiegeExpression siegeExpr

generateClojureExpression _ = 
  throwError $ CodeGenError "Unsupported expression in Clojure generation"

-- | Generate Clojure literal
generateClojureLiteral :: Literal -> Text
generateClojureLiteral (FloatLit f) = T.pack $ show f
generateClojureLiteral GoldenRatioLit = "1.618033988749895"
generateClojureLiteral PiLit = "Math/PI"
generateClojureLiteral TauLit = "(* 2 Math/PI)"

-- | Generate Clojure SIEGE expression
generateClojureSiegeExpression :: SiegeExpression -> Text
generateClojureSiegeExpression (SiegeConst val) = T.pack $ show val
generateClojureSiegeExpression SiegeTime = "time"
generateClojureSiegeExpression (SiegeSin expr) = "(Math/sin " <> generateClojureSiegeExpression expr <> ")"

-- | Empty code generation state
emptyState :: CodeGenState
emptyState = CodeGenState
  { indentLevel = 0
  , currentModule = Nothing
  , imports = []
  , definitions = []
  }
