-- | Main module for the Glados project.
-- This module handles the program entry point and orchestrates the parsing and processing of Lisp expressions.
module Main (main) where

import Lib (checkArgs, litostr, needParenthese, backToFile)
import Parser.ParserLispSExp (parseSExpr, pProgram)
import Text.Megaparsec
import Parser.ParserSExpAST (parseAST)
import HandleAST.HandleAST (handleAST)
import CheckLisp (checkLisp)
import System.Exit (exitWith, ExitCode(..))

-- | The main function reads the input, processes it, and handles the AST.
-- It performs the following steps:
-- 1. Reads input from arguments or standard input.
-- 2. Converts the input into a single string.
-- 3. Ensures parentheses are balanced.
-- 4. Parses the input into an S-expression.
-- 5. Converts the S-expression into an abstract syntax tree (AST).
-- 6. Processes the AST and prints the result.
main :: IO ()
main = do
  input <- checkArgs
  putStrLn ("FILE CONTENT: " ++ (litostr input))
  let result = parse pProgram "Input" (litostr input)
  case result of
    Left err -> putStrLn (errorBundlePretty err)
    Right expr -> putStrLn "good" >> print expr
