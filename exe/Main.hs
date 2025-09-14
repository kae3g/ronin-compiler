{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.FilePath

import Ronin.Lexer
import Ronin.Parser
import Ronin.AST
import Ronin.TypeChecker
import Ronin.CodeGen

-- | Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn usage
    ["--help"] -> putStrLn usage
    ["--version"] -> putStrLn version
    ["compile", "--target", targetStr, input] -> do
      case parseTarget targetStr of
        Just target -> compileFile target input
        Nothing -> do
          putStrLn $ "Error: Unknown target '" ++ targetStr ++ "'"
          exitWith (ExitFailure 1)
    ["compile", "--target", targetStr, input, "-o", output] -> do
      case parseTarget targetStr of
        Just target -> compileFileTo target input output
        Nothing -> do
          putStrLn $ "Error: Unknown target '" ++ targetStr ++ "'"
          exitWith (ExitFailure 1)
    ["typecheck", input] -> typecheckFile input
    ["parse", "--pretty", input] -> parseFile input
    _ -> do
      putStrLn "Error: Invalid arguments"
      putStrLn usage
      exitWith (ExitFailure 1)

-- | Usage information
usage :: String
usage = unlines
  [ "RONIN Compiler v0.1.0"
  , ""
  , "Usage:"
  , "  ronin compile --target <target> <input> [-o <output>]"
  , "  ronin typecheck <input>"
  , "  ronin parse --pretty <input>"
  , "  ronin --help"
  , "  ronin --version"
  , ""
  , "Targets:"
  , "  haskell  - Generate Haskell code"
  , "  rust     - Generate Rust code"
  , "  clojure  - Generate Clojure code"
  , ""
  , "Examples:"
  , "  ronin compile --target haskell example.ronin"
  , "  ronin compile --target rust example.ronin -o output.rs"
  , "  ronin typecheck example.ronin"
  , "  ronin parse --pretty example.ronin"
  ]

-- | Version information
version :: String
version = "RONIN Compiler v0.1.0 - MAITREYA DAW Language Compiler"

-- | Parse target string
parseTarget :: String -> Maybe Target
parseTarget "haskell" = Just Haskell
parseTarget "rust" = Just Rust
parseTarget "clojure" = Just Clojure
parseTarget "wasm" = Just WebAssembly
parseTarget _ = Nothing

-- | Compile file to target language
compileFile :: Target -> FilePath -> IO ()
compileFile target input = do
  result <- compileFileTo target input (replaceExtension input (targetExtension target))
  case result of
    Left err -> do
      putStrLn $ "Compilation error: " ++ show err
      exitWith (ExitFailure 1)
    Right _ -> putStrLn "Compilation successful"

-- | Compile file to specific output
compileFileTo :: Target -> FilePath -> FilePath -> IO (Either String ())
compileFileTo target input output = do
  -- Read input file
  inputText <- TIO.readFile input
  
  -- Parse
  case parseModule inputText of
    Left parseErr -> return $ Left $ "Parse error: " ++ show parseErr
    Right ast -> do
      -- Type check
      case typeCheck ast of
        Left typeErr -> return $ Left $ "Type error: " ++ show typeErr
        Right _ -> do
          -- Generate code
          case generateCode target ast of
            Left codeErr -> return $ Left $ "Code generation error: " ++ show codeErr
            Right code -> do
              -- Write output
              TIO.writeFile output code
              return $ Right ()

-- | Type check file
typecheckFile :: FilePath -> IO ()
typecheckFile input = do
  inputText <- TIO.readFile input
  
  case parseModule inputText of
    Left parseErr -> do
      putStrLn $ "Parse error: " ++ show parseErr
      exitWith (ExitFailure 1)
    Right ast -> do
      case typeCheck ast of
        Left typeErr -> do
          putStrLn $ "Type error: " ++ show typeErr
          exitWith (ExitFailure 1)
        Right _ -> do
          putStrLn "Type checking successful"

-- | Parse and pretty print file
parseFile :: FilePath -> IO ()
parseFile input = do
  inputText <- TIO.readFile input
  
  case parseModule inputText of
    Left parseErr -> do
      putStrLn $ "Parse error: " ++ show parseErr
      exitWith (ExitFailure 1)
    Right ast -> do
      putStrLn "Parse successful:"
      putStrLn $ show ast

-- | Get file extension for target
targetExtension :: Target -> String
targetExtension Haskell = "hs"
targetExtension Rust = "rs"
targetExtension Clojure = "clj"
targetExtension WebAssembly = "wat"
