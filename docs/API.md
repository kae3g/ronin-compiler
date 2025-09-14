# RONIN Compiler API Documentation

## Overview

The RONIN Compiler provides a comprehensive API for parsing, type-checking, and code generation of RONIN language programs. This document describes the main modules and their interfaces.

## Core Modules

### Ronin.Lexer

The lexical analysis module for tokenizing RONIN source code.

```haskell
-- Main functions
tokenize :: Text -> Either ParseError [Token]
lexer :: Parser [Token]

-- Token types
data TokenType = KWModule | KWWhere | ... | EOF
data Token = Token { tokenType :: TokenType, tokenText :: Text, tokenPos :: SourcePos }
```

**Key Functions:**
- `tokenize`: Tokenize text input into a list of tokens
- `lexer`: Parser for tokenizing RONIN source code

### Ronin.Parser

The parsing module for building Abstract Syntax Trees from tokens.

```haskell
-- Main functions
parseModule :: Parser Module
parseExpression :: Parser Expression
parseType :: Parser Type
parseDeclaration :: Parser Declaration
parsePattern :: Parser Pattern
parseSiegeExpression :: Parser SiegeExpression

-- Error handling
parseError :: ParseError -> String
```

**Key Functions:**
- `parseModule`: Parse a complete RONIN module
- `parseExpression`: Parse RONIN expressions
- `parseType`: Parse type annotations
- `parseSiegeExpression`: Parse SIEGE mathematical expressions

### Ronin.AST

The Abstract Syntax Tree definition for RONIN programs.

```haskell
-- Main data types
data Module = Module { modName :: Text, modImports :: [Import], modDeclarations :: [Declaration] }
data Declaration = FunctionDecl Text [Pattern] Expression Type Position | ...
data Expression = Lit Literal Position | Var Text Position | App Expression Expression Position | ...
data Type = TypeVar Text Position | TypeCon Text [Type] Position | TypeFun Type Type Position | ...
data SiegeExpression = SiegeConst Double | SiegeTime | SiegeSin SiegeExpression | ...

-- Helper functions
moduleName :: Module -> Text
declarationName :: Declaration -> Maybe Text
expressionType :: Expression -> Maybe Type
typeVariables :: Type -> [Text]
```

**Key Data Types:**
- `Module`: Complete RONIN module
- `Declaration`: Function, type, and data declarations
- `Expression`: RONIN expressions including SIEGE integration
- `Type`: Type system with higher-kinded types
- `SiegeExpression`: Mathematical audio processing expressions

### Ronin.TypeChecker

The type checking module for static analysis of RONIN programs.

```haskell
-- Main functions
typeCheck :: Module -> Either TypeError TypeInfo
inferType :: Expression -> TypeChecker Type
checkType :: Expression -> Type -> TypeChecker ()

-- Error types
data TypeError = TypeMismatch Type Type Position | UndefinedVariable Text Position | ...

-- Type environment
data TypeEnvironment = TypeEnvironment { varTypes :: Map Text Type, ... }
```

**Key Functions:**
- `typeCheck`: Type check a complete module
- `inferType`: Infer the type of an expression
- `checkType`: Check an expression against a given type

### Ronin.CodeGen

The code generation module for compiling RONIN to target languages.

```haskell
-- Main functions
generateCode :: Target -> Module -> Either CodeGenError Text
generateHaskell :: Module -> Either CodeGenError Text
generateRust :: Module -> Either CodeGenError Text
generateClojure :: Module -> Either CodeGenError Text

-- Targets
data Target = Haskell | Rust | Clojure | WebAssembly

-- Error types
data CodeGenError = UnsupportedTarget Target | UnsupportedExpression Expression | ...
```

**Key Functions:**
- `generateCode`: Generate code for any supported target
- `generateHaskell`: Generate Haskell code specifically
- `generateRust`: Generate Rust code specifically
- `generateClojure`: Generate Clojure code specifically

## Usage Examples

### Basic Compilation Pipeline

```haskell
import Ronin.Lexer
import Ronin.Parser
import Ronin.TypeChecker
import Ronin.CodeGen

-- Complete compilation pipeline
compileToHaskell :: Text -> Either String Text
compileToHaskell source = do
  -- Tokenize
  tokens <- tokenize source
  
  -- Parse
  ast <- parseModule source
  
  -- Type check
  _ <- typeCheck ast
  
  -- Generate code
  generateHaskell ast
```

### Type Checking Only

```haskell
-- Type check without code generation
typecheckOnly :: Text -> Either TypeError TypeInfo
typecheckOnly source = do
  ast <- parseModule source
  typeCheck ast
```

### Custom Code Generation

```haskell
-- Generate for multiple targets
generateAll :: Module -> [(Target, Either CodeGenError Text)]
generateAll ast = 
  [ (Haskell, generateHaskell ast)
  , (Rust, generateRust ast)
  , (Clojure, generateClojure ast)
  ]
```

## Error Handling

The RONIN Compiler provides comprehensive error handling at each stage:

### Parse Errors
```haskell
data ParseError = ParseError { errorPos :: SourcePos, errorMsg :: String }
```

### Type Errors
```haskell
data TypeError = 
  TypeMismatch Type Type Position
  | UndefinedVariable Text Position
  | UndefinedTypeConstructor Text Position
  | CircularTypeDefinition Text Position
  | InvalidPattern Pattern Type Position
  | CannotUnify Type Type Position
  | KindMismatch Type Type Position
  | ArityMismatch Int Int Position
  | NonExhaustivePatterns [Pattern] Position
```

### Code Generation Errors
```haskell
data CodeGenError = 
  UnsupportedTarget Target
  | UnsupportedExpression Expression
  | UnsupportedType Type
  | CodeGenError Text
```

## Advanced Features

### SIEGE Expression Integration

SIEGE expressions provide mathematical audio processing capabilities:

```haskell
-- SIEGE expression types
data SiegeExpression = 
  SiegeConst Double
  | SiegeTime
  | SiegeSin SiegeExpression
  | SiegeCos SiegeExpression
  | SiegeAdd SiegeExpression SiegeExpression
  | SiegeMul SiegeExpression SiegeExpression
  | SiegeIntegral SiegeExpression
  | SiegeDerivative SiegeExpression
  | SiegeDelay SiegeExpression Double
  | SiegeFilter SiegeExpression Double Double
  | SiegeEnvelope SiegeExpression Double Double Double Double
  | SiegeGolden SiegeExpression
  | ...
```

### Type System

RONIN features a sophisticated type system with:

- **Higher-kinded types**: Support for type constructors
- **Type inference**: Automatic type deduction
- **Pattern matching**: Exhaustive pattern checking
- **Kind system**: Type-level programming support

### Multi-target Code Generation

The compiler supports multiple output targets:

- **Haskell**: Type-safe functional audio processing
- **Rust**: High-performance real-time audio engines
- **Clojure**: Reactive user interface bindings
- **WebAssembly**: Browser-compatible audio processing

## Integration with MAITREYA DAW

The RONIN Compiler is designed to integrate seamlessly with the MAITREYA DAW ecosystem:

- **SIEGE Integration**: Direct support for SIEGE mathematical expressions
- **Golden Ratio Support**: Built-in mathematical constants and relationships
- **Audio Types**: Native audio signal types and processing functions
- **Multi-language Architecture**: Generates code for all MAITREYA components

## Performance Considerations

- **Lazy Evaluation**: Efficient evaluation of audio processing graphs
- **SIMD Optimization**: Automatic vectorization for performance targets
- **Memory Management**: Optimized memory layout for real-time processing
- **Dead Code Elimination**: Removal of unused audio processing chains

---

*This API documentation is part of the RONIN Compiler for MAITREYA DAW - the gentle revolution in audio programming.* 💚💙
