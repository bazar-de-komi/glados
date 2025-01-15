-- | Main module for the Glados project.
--
-- This module serves as the entry point for the Glados program. It orchestrates
-- the reading, parsing, and processing of Kleftis expressions, transforming
-- them into an abstract syntax tree (AST) for further computation or evaluation.

module Main (main) where

import Lib (checkArgs, litostr, giveFileName)
import Parser.ParserKleftisSExp (pProgram)
import Parser.ParserSExpAST (parseFinalAST)
import Text.Megaparsec
import GenerateBytecode (generateInstructionsList)
import System.Environment
import System.IO
import System.Exit (exitWith, exitSuccess, ExitCode(ExitFailure))

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
  args <- getArgs
  input <- checkArgs args
  if "-c" `elem` args then justCompile (litostr input) (giveFileName args "-c") else
    if "-v" `elem` args then justVM (litostr input) else
      glados(litostr input)


justCompile :: String -> String -> IO ()
justCompile str "" = do
  let result = parse pProgram "Input" (str)
  case result of
    Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
    Right expr -> case parseFinalAST expr of
      Left errr -> putStrLn errr >> exitWith (ExitFailure 84)
      Right ast -> mapM_ print (generateInstructionsList ast) >> exitSuccess
justCompile str fileName = do
  handle <- openFile fileName WriteMode
  let result = parse pProgram "Input" str
  case result of
    Left err -> hClose handle >> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
    Right expr -> case parseFinalAST expr of
      Left errr -> hClose handle >> putStrLn errr >> exitWith (ExitFailure 84)
      Right ast ->
        mapM_ (hPutStrLn handle . show) (generateInstructionsList ast)
          >> hClose handle >> exitSuccess

justVM :: String -> IO()
justVM str = do
  let result = parse pProgram "Input" (str)
  case result of
    Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
    Right expr -> case parseFinalAST expr of
      Left errr -> putStrLn errr >> exitWith (ExitFailure 84)
      Right ast -> mapM_ print (generateInstructionsList ast) >> exitSuccess

glados :: String -> IO()
glados str  = do
  let result = parse pProgram "Input" (str)
  case result of
    Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
    Right expr -> case parseFinalAST expr of
      Left errr -> putStrLn errr >> exitWith (ExitFailure 84)
      Right ast -> mapM_ print (generateInstructionsList ast) >> exitSuccess

