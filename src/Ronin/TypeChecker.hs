{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Ronin.TypeChecker
  ( TypeChecker
  , TypeError(..)
  , TypeInfo(..)
  , typeCheck
  , inferType
  , checkType
  , typeEnvironment
  ) where

import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)

import Ronin.AST

-- | Type checking monad
type TypeChecker = StateT TypeEnvironment (Except TypeError)

-- | Type checking errors
data TypeError
  = TypeMismatch Type Type Position
  | UndefinedVariable Text Position
  | UndefinedTypeConstructor Text Position
  | CircularTypeDefinition Text Position
  | InvalidPattern Pattern Type Position
  | CannotUnify Type Type Position
  | KindMismatch Type Type Position
  | ArityMismatch Int Int Position
  | NonExhaustivePatterns [Pattern] Position
  deriving (Show, Eq)

-- | Type information
data TypeInfo = TypeInfo
  { typeEnv :: TypeEnvironment
  , typeConstraints :: [TypeConstraint]
  , typeSubstitutions :: Map Text Type
  } deriving (Show, Eq)

-- | Type environment
data TypeEnvironment = TypeEnvironment
  { varTypes :: Map Text Type
  , typeConstructors :: Map Text TypeSignature
  , dataConstructors :: Map Text TypeSignature
  , currentModule :: Maybe Text
  } deriving (Show, Eq)

-- | Type constraint
data TypeConstraint = TypeConstraint
  { constraintLeft :: Type
  , constraintRight :: Type
  , constraintPosition :: Position
  } deriving (Show, Eq)

-- | Type signature
data TypeSignature = TypeSignature
  { sigType :: Type
  , sigKind :: Kind
  , sigArity :: Int
  } deriving (Show, Eq)

-- | Kind system for higher-kinded types
data Kind
  = Star                    -- Basic types
  | KFun Kind Kind          -- Function kinds
  | KRow                    -- Row kinds for records
  deriving (Show, Eq)

-- | Type check a module
typeCheck :: Module -> Either TypeError TypeInfo
typeCheck module_ = runExcept $ evalStateT (checkModule module_) emptyEnvironment

-- | Check a module
checkModule :: Module -> TypeChecker TypeInfo
checkModule (Module name imports decls pos) = do
  -- Set current module
  modify $ \env -> env { currentModule = Just name }
  
  -- Process imports
  mapM_ processImport imports
  
  -- Check declarations
  mapM_ checkDeclaration decls
  
  -- Get final environment
  env <- get
  
  return $ TypeInfo env [] Map.empty

-- | Process import
processImport :: Import -> TypeChecker ()
processImport (Import moduleName alias items pos) = do
  -- In a real implementation, this would load the imported module
  -- and add its symbols to the environment
  return ()

-- | Check declaration
checkDeclaration :: Declaration -> TypeChecker ()
checkDeclaration (FunctionDecl name patterns expr typ pos) = do
  -- Check if function is already defined
  env <- get
  case Map.lookup name (varTypes env) of
    Just existingType -> throwError $ TypeMismatch typ existingType pos
    Nothing -> do
      -- Infer type from patterns and expression
      inferredType <- inferFunctionType patterns expr
      
      -- Check against annotation if provided
      case typ of
        TypeVar _ _ -> do
          -- No annotation, use inferred type
          modify $ \e -> e { varTypes = Map.insert name inferredType (varTypes e) }
        _ -> do
          -- Check annotation matches inferred type
          when (not $ typesEqual inferredType typ) $
            throwError $ TypeMismatch typ inferredType pos
          modify $ \e -> e { varTypes = Map.insert name typ (varTypes e) }

checkDeclaration (TypeDecl name vars typ pos) = do
  -- Check type declaration
  env <- get
  case Map.lookup name (typeConstructors env) of
    Just existingSig -> throwError $ TypeMismatch typ (sigType existingSig) pos
    Nothing -> do
      let kind = inferKind typ
      let arity = length vars
      let sig = TypeSignature typ kind arity
      modify $ \e -> e { typeConstructors = Map.insert name sig (typeConstructors e) }

checkDeclaration (DataDecl name vars constructors pos) = do
  -- Check data declaration
  mapM_ (checkConstructor name vars) constructors

checkDeclaration (ModuleDecl name decls pos) = do
  -- Check nested module
  mapM_ checkDeclaration decls

-- | Check constructor
checkConstructor :: Text -> [TypeVariable] -> TypeConstructor -> TypeChecker ()
checkConstructor dataName vars (TypeConstructor name args pos) = do
  -- Build constructor type
  let constructorType = foldr TypeFun (TypeCon dataName (map (TypeVar . tvName) vars) pos) args
  
  -- Add to environment
  modify $ \e -> e { dataConstructors = Map.insert name (TypeSignature constructorType Star (length args)) (dataConstructors e) }

-- | Infer function type
inferFunctionType :: [Pattern] -> Expression -> TypeChecker Type
inferFunctionType patterns expr = do
  -- Infer types of patterns
  patternTypes <- mapM inferPatternType patterns
  
  -- Infer type of expression
  exprType <- inferExpressionType expr
  
  -- Build function type
  let argType = if length patternTypes == 1 then head patternTypes else TypeTuple patternTypes (position (head patterns))
  return $ TypeFun argType exprType (position expr)

-- | Infer pattern type
inferPatternType :: Pattern -> TypeChecker Type
inferPatternType (VarPattern name pos) = do
  -- Create fresh type variable
  return $ TypeVar name pos

inferPatternType (LitPattern lit pos) = do
  return $ literalType lit

inferPatternType (ConPattern name args pos) = do
  -- Look up constructor
  env <- get
  case Map.lookup name (dataConstructors env) of
    Just sig -> do
      let constructorType = sigType sig
      -- Apply arguments
      return $ applyConstructor constructorType args
    Nothing -> throwError $ UndefinedTypeConstructor name pos

inferPatternType (WildcardPattern pos) = do
  -- Create fresh type variable
  return $ TypeVar "_" pos

inferPatternType (ListPattern patterns pos) = do
  if null patterns
    then return $ TypeList (TypeVar "a" pos) pos
    else do
      elementTypes <- mapM inferPatternType patterns
      let elementType = head elementTypes  -- Assume all elements have same type
      return $ TypeList elementType pos

inferPatternType (TuplePattern patterns pos) = do
  patternTypes <- mapM inferPatternType patterns
  return $ TypeTuple patternTypes pos

inferPatternType (RecordPattern fields pos) = do
  fieldTypes <- mapM (inferPatternType . snd) fields
  let fieldNames = map fst fields
  return $ TypeRecord (zip fieldNames fieldTypes) pos

inferPatternType (AsPattern pattern name pos) = do
  patternType <- inferPatternType pattern
  return patternType

-- | Infer expression type
inferExpressionType :: Expression -> TypeChecker Type
inferExpressionType (Lit lit pos) = do
  return $ literalType lit

inferExpressionType (Var name pos) = do
  env <- get
  case Map.lookup name (varTypes env) of
    Just typ -> return typ
    Nothing -> throwError $ UndefinedVariable name pos

inferExpressionType (App func arg pos) = do
  funcType <- inferExpressionType func
  argType <- inferExpressionType arg
  
  case funcType of
    TypeFun inputType outputType _ -> do
      -- Check argument type matches
      when (not $ typesEqual argType inputType) $
        throwError $ TypeMismatch argType inputType pos
      return outputType
    _ -> throwError $ TypeMismatch funcType (TypeFun argType (TypeVar "b" pos) pos) pos

inferExpressionType (Lambda patterns expr pos) = do
  patternTypes <- mapM inferPatternType patterns
  exprType <- inferExpressionType expr
  
  let argType = if length patternTypes == 1 then head patternTypes else TypeTuple patternTypes pos
  return $ TypeFun argType exprType pos

inferExpressionType (Let decls expr pos) = do
  -- Check declarations in local scope
  mapM_ checkDeclaration decls
  inferExpressionType expr

inferExpressionType (Case expr branches pos) = do
  exprType <- inferExpressionType expr
  
  -- Check all branches have same type
  branchTypes <- mapM (\(pattern, branchExpr) -> do
    patternType <- inferPatternType pattern
    when (not $ typesEqual patternType exprType) $
      throwError $ TypeMismatch patternType exprType pos
    inferExpressionType branchExpr
    ) branches
  
  let resultType = head branchTypes
  return resultType

inferExpressionType (If cond thenExpr elseExpr pos) = do
  condType <- inferExpressionType cond
  thenType <- inferExpressionType thenExpr
  elseType <- inferExpressionType elseExpr
  
  -- Check condition is boolean
  when (not $ typesEqual condType (TypeCon "Bool" [] pos)) $
    throwError $ TypeMismatch condType (TypeCon "Bool" [] pos) pos
  
  -- Check branches have same type
  when (not $ typesEqual thenType elseType) $
    throwError $ TypeMismatch thenType elseType pos
  
  return thenType

inferExpressionType (Op op left right pos) = do
  leftType <- inferExpressionType left
  rightType <- inferExpressionType right
  
  case op of
    OpPlus -> checkNumericOperation leftType rightType pos
    OpMinus -> checkNumericOperation leftType rightType pos
    OpTimes -> checkNumericOperation leftType rightType pos
    OpDivide -> checkNumericOperation leftType rightType pos
    OpEqual -> return $ TypeCon "Bool" [] pos
    OpNotEqual -> return $ TypeCon "Bool" [] pos
    OpLess -> checkComparisonOperation leftType rightType pos
    OpGreater -> checkComparisonOperation leftType rightType pos
    OpLessEqual -> checkComparisonOperation leftType rightType pos
    OpGreaterEqual -> checkComparisonOperation leftType rightType pos
    OpAnd -> checkBooleanOperation leftType rightType pos
    OpOr -> checkBooleanOperation leftType rightType pos
    _ -> return $ TypeVar "a" pos

inferExpressionType (List exprs pos) = do
  if null exprs
    then return $ TypeList (TypeVar "a" pos) pos
    else do
      elementTypes <- mapM inferExpressionType exprs
      let elementType = head elementTypes
      return $ TypeList elementType pos

inferExpressionType (Tuple exprs pos) = do
  types <- mapM inferExpressionType exprs
  return $ TypeTuple types pos

inferExpressionType (Record fields pos) = do
  fieldTypes <- mapM (inferExpressionType . snd) fields
  let fieldNames = map fst fields
  return $ TypeRecord (zip fieldNames fieldTypes) pos

inferExpressionType (RecordAccess expr field pos) = do
  exprType <- inferExpressionType expr
  case exprType of
    TypeRecord fields _ -> do
      case lookup field fields of
        Just fieldType -> return fieldType
        Nothing -> throwError $ UndefinedVariable field pos
    _ -> throwError $ TypeMismatch exprType (TypeRecord [] pos) pos

inferExpressionType (Siege siegeExpr pos) = do
  -- SIEGE expressions always produce audio signals
  return $ TypeCon "AudioSignal" [] pos

inferExpressionType (Annotated expr typ pos) = do
  inferredType <- inferExpressionType expr
  when (not $ typesEqual inferredType typ) $
    throwError $ TypeMismatch inferredType typ pos
  return typ

-- | Check numeric operation
checkNumericOperation :: Type -> Type -> Position -> TypeChecker Type
checkNumericOperation left right pos = do
  when (not $ isNumericType left) $
    throwError $ TypeMismatch left (TypeCon "Float" [] pos) pos
  when (not $ isNumericType right) $
    throwError $ TypeMismatch right (TypeCon "Float" [] pos) pos
  return left

-- | Check comparison operation
checkComparisonOperation :: Type -> Type -> Position -> TypeChecker Type
checkComparisonOperation left right pos = do
  when (not $ typesEqual left right) $
    throwError $ TypeMismatch left right pos
  return $ TypeCon "Bool" [] pos

-- | Check boolean operation
checkBooleanOperation :: Type -> Type -> Position -> TypeChecker Type
checkBooleanOperation left right pos = do
  let boolType = TypeCon "Bool" [] pos
  when (not $ typesEqual left boolType) $
    throwError $ TypeMismatch left boolType pos
  when (not $ typesEqual right boolType) $
    throwError $ TypeMismatch right boolType pos
  return boolType

-- | Get literal type
literalType :: Literal -> Type
literalType (IntLit _) = TypeCon "Int" [] (Position 0 0 Nothing)
literalType (FloatLit _) = TypeCon "Float" [] (Position 0 0 Nothing)
literalType (CharLit _) = TypeCon "Char" [] (Position 0 0 Nothing)
literalType (StringLit _) = TypeCon "String" [] (Position 0 0 Nothing)
literalType (BoolLit _) = TypeCon "Bool" [] (Position 0 0 Nothing)
literalType GoldenRatioLit = TypeCon "Float" [] (Position 0 0 Nothing)
literalType PiLit = TypeCon "Float" [] (Position 0 0 Nothing)
literalType TauLit = TypeCon "Float" [] (Position 0 0 Nothing)
literalType EulerLit = TypeCon "Float" [] (Position 0 0 Nothing)

-- | Check if type is numeric
isNumericType :: Type -> Bool
isNumericType (TypeCon "Int" _ _) = True
isNumericType (TypeCon "Float" _ _) = True
isNumericType _ = False

-- | Check if types are equal
typesEqual :: Type -> Type -> Bool
typesEqual (TypeVar name1 _) (TypeVar name2 _) = name1 == name2
typesEqual (TypeCon name1 args1 _) (TypeCon name2 args2 _) = 
  name1 == name2 && all (uncurry typesEqual) (zip args1 args2)
typesEqual (TypeFun arg1 res1 _) (TypeFun arg2 res2 _) = 
  typesEqual arg1 arg2 && typesEqual res1 res2
typesEqual (TypeTuple types1 _) (TypeTuple types2 _) = 
  all (uncurry typesEqual) (zip types1 types2)
typesEqual (TypeList type1 _) (TypeList type2 _) = typesEqual type1 type2
typesEqual (TypeRecord fields1 _) (TypeRecord fields2 _) = 
  length fields1 == length fields2 &&
  all (\(name1, type1) -> 
    case lookup name1 fields2 of
      Just type2 -> typesEqual type1 type2
      Nothing -> False
    ) fields1
typesEqual _ _ = False

-- | Infer kind of type
inferKind :: Type -> Kind
inferKind (TypeVar _ _) = Star
inferKind (TypeCon _ [] _) = Star
inferKind (TypeCon _ args _) = 
  foldr (\_ k -> KFun Star k) Star args
inferKind (TypeFun _ _ _) = Star
inferKind (TypeTuple _ _) = Star
inferKind (TypeList _ _) = Star
inferKind (TypeRecord _ _) = Star

-- | Apply constructor with arguments
applyConstructor :: Type -> [Pattern] -> Type
applyConstructor constructorType patterns = 
  foldl (\t _ -> case t of TypeFun _ res _ -> res; _ -> t) constructorType patterns

-- | Empty type environment
emptyEnvironment :: TypeEnvironment
emptyEnvironment = TypeEnvironment
  { varTypes = Map.empty
  , typeConstructors = Map.empty
  , dataConstructors = Map.empty
  , currentModule = Nothing
  }

-- | Check type against expression
checkType :: Expression -> Type -> TypeChecker ()
checkType expr expectedType = do
  inferredType <- inferExpressionType expr
  when (not $ typesEqual inferredType expectedType) $
    throwError $ TypeMismatch inferredType expectedType (position expr)

-- | Get position from AST node
position :: (Located a) => Located a -> Position
position = position

-- | Type environment getter
typeEnvironment :: TypeChecker TypeEnvironment
typeEnvironment = get
