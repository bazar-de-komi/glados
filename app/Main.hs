-- | Main module for the Glados project.
--
-- This module serves as the entry point for the Glados program. It orchestrates
-- the reading, parsing, and processing of Kleftis expressions, transforming
-- them into an abstract syntax tree (AST) for further computation or evaluation.

module Main (main) where

import Lib (checkArgs, litostr, giveFileName)
import Parser.ParserKleftisSExp (pProgram)
import Parser.ParserCompilVM (pProgramInst)
import Parser.ParserSExpAST (parseFinalAST, lastCheck)
import Text.Megaparsec
import Compiler.GenerateBytecode (generateInstructionsList)
import VM.RunVM (runVM)
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

-- | Compiles a Kleftis program into a list of bytecode instructions.
--
-- This function takes the program source code as a string and an optional output
-- filename. It parses the input into an abstract syntax tree (AST), generates
-- the corresponding bytecode instructions, and writes them to the specified file.
--
-- If no filename is provided, the instructions are printed to the standard output.
--
-- ==== Error Handling:
-- - If parsing fails, an error message is printed, and the program exits with code 84.
-- - If semantic analysis of the AST fails, an appropriate error message is printed,
--   and the program exits with code 84.
--
-- ==== Parameters:
-- - `String`: The program source code as a string.
-- - `String`: The name of the output file for the compiled bytecode.
--
-- ==== Example:
-- Compiling the input:
--
-- > (define x 42)
--
-- Results in bytecode similar to:
--
-- > STORE_CONST 42
-- > STORE_VAR x
justCompile :: String -> String -> IO ()
justCompile str "" =
  case (parse pProgram "Input" (str)) of
    Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
    Right expr -> case lastCheck (parseFinalAST expr) of
      Left errr -> putStrLn errr >> exitWith (ExitFailure 84)
      Right ast -> mapM_ print (generateInstructionsList ast) >> exitSuccess
justCompile str fileName = do
  handle <- openFile fileName WriteMode
  case (parse pProgram "Input" str) of
    Left err -> hClose handle >> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
    Right expr -> case lastCheck (parseFinalAST expr) of
      Left errr -> hClose handle >> putStrLn errr >> exitWith (ExitFailure 84)
      Right ast ->
        mapM_ (hPutStrLn handle . show) (generateInstructionsList ast)
          >> hClose handle >> exitSuccess

-- | Executes a list of bytecode instructions using the virtual machine (VM).
--
-- This function takes a string representing bytecode instructions, parses them
-- into a list of `Instruction`s, and executes them using the virtual machine.
-- The result of the execution is printed to the standard output.
--
-- ==== Error Handling:
-- - If parsing the bytecode fails, an error message is printed, and the program exits with code 84.
--
-- ==== Parameters:
-- - `String`: The bytecode instructions as a string.
--
-- ==== Example:
-- Executing the input:
--
-- > STORE_CONST 42
-- > STORE_VAR x
-- > LOAD_VAR x
--
-- Prints the result of the virtual machine execution.
justVM :: String -> IO()
justVM str =
  case (parse pProgramInst "Input" (str)) of
    Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
    Right expr -> print (runVM expr) >> exitSuccess

-- | Orchestrates the entire workflow of parsing, semantic analysis, compilation, and execution.
--
-- This function processes a Kleftis program in its entirety:
-- 1. Parses the program into an abstract syntax tree (AST).
-- 2. Performs semantic analysis on the AST.
-- 3. Generates bytecode instructions from the AST.
-- 4. Executes the bytecode using the virtual machine (VM).
--
-- The function is designed to handle errors at each step and print appropriate
-- messages if issues arise.
--
-- ==== Error Handling:
-- - If parsing the program fails, an error message is printed, and the program exits with code 84.
-- - If semantic analysis fails, a semantic error message is printed, and the program exits with code 84.
--
-- ==== Parameters:
-- - `String`: The program source code as a string.
--
-- ==== Example:
-- Given the input:
--
-- > (define x 42)
--
-- This function:
-- 1. Parses it into an S-expression: `List [Atom "define", Atom "x", SEInt 42]`.
-- 2. Converts it into an AST: `SDefine "x" (SType "int") (SInt 42)`.
-- 3. Generates bytecode: `STORE_CONST 42`, `STORE_VAR x`.
-- 4. Executes the bytecode and prints the result.
glados :: String -> IO()
glados str =
  case (parse pProgram "Input" (str)) of
    Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
    Right expr -> case lastCheck (parseFinalAST expr) of
      Left errr -> putStrLn errr >> exitWith (ExitFailure 84)
      Right ast -> print (runVM (generateInstructionsList ast)) >> exitSuccess

