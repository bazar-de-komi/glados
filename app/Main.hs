-- | Main module for the Glados project.
--
-- This module serves as the entry point for the Glados program. It orchestrates
-- the reading, parsing, and processing of Kleftis expressions, transforming
-- them into an abstract syntax tree (AST) for further computation or evaluation.

{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lib (checkArgs, litostr)
import Parser.ParserKleftisSExp (pProgram)
import Parser.ParserSExpAST (parseFinalAST)
import Text.Megaparsec
import GenerateBytecode (generateInstructionsList)
import Structure (VM (..))
import VM.Execute (runVM)

-- | The `main` function is the entry point of the program.
--
-- It performs the following steps:
--
-- 1. **Read Input**:
--    - Uses `checkArgs` to read input from the command line or standard input.
--    - Converts the input into a single string using `litostr`.
--
-- 2. **Parse Input**:
--    - Uses `pProgram` to parse the input string into an S-expression (`SExpr`).
--    - Handles parsing errors gracefully, displaying an error message if parsing fails.
--
-- 3. **Convert to AST**:
--    - Uses `parseFinalAST` to convert the parsed S-expression into an abstract syntax tree (AST).
--    - Handles conversion errors, displaying an appropriate error message if conversion fails.
--
-- 4. **Print Result**:
--    - If parsing and conversion are successful, prints the resulting AST to the standard output.
--
-- ==== Error Handling:
-- - If parsing (`pProgram`) fails, the program prints a detailed error message from Megaparsec.
-- - If AST conversion (`parseFinalAST`) fails, the program prints a semantic error message.
--
-- ==== Example Workflow:
-- Given the input:
--
-- > (define x 42)
--
-- The program:
-- 1. Reads the input.
-- 2. Parses it into an S-expression: `List [Atom "define", Atom "x", SEInt 42]`.
-- 3. Converts it into an AST: `SDefine "x" (SType "int") (SInt 42)`.
-- 4. Prints the resulting AST.
--
-- ==== Code Example:
-- The `main` function relies on:
--
-- - `checkArgs`: Reads input from the command line or standard input.
-- - `litostr`: Converts the input into a string format.
-- - `pProgram`: Parses the input into an S-expression.
-- - `parseFinalAST`: Converts the parsed S-expression into an AST.
main :: IO ()
main = do
  input <- checkArgs
  let result = parse pProgram "Input" (litostr input)
  case result of
    Left err -> putStrLn (errorBundlePretty err)
    Right expr -> case parseFinalAST expr of
      Left errror -> putStrLn errror
      Right ast ->
        mapM_ print (generateInstructionsList ast) >>
        let vm = runVM (generateInstructionsList ast)
        in print vm >> case stack vm of
              (top:_) -> putStrLn $ "Last value on stack: " ++ show top
              [] -> putStrLn "Stack is empty"

-- main :: IO ()
-- main =
--   let vm = initializeVM exampleProgram
--   in print (runVM vm)
