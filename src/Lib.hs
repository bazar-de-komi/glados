-- | Utility functions for handling arguments, input, and parentheses.
-- This module provides functions to:
-- 1. Read command-line arguments or standard input.
-- 2. Convert input into a single string.
-- 3. Handle parentheses in Lisp-like expressions.

module Lib (checkArgs, litostr, needParenthese, checkFlag, tailOf, whilegetline, checkparenthese, checkNotEnd, checkAllString) where

import System.IO
import System.Environment

-- | Check if the '-i' flag is present in the arguments.
--
-- Returns 'True' if the flag is found, otherwise 'False'.
checkFlag :: [String] -> Bool
checkFlag args = "-i" `elem` args

-- | Get the last element of a list of strings.
--
-- Useful for extracting filenames from command-line arguments.
tailOf :: [String] -> String
tailOf [] = []
tailOf (a:b)
  | b == [] = a
  | otherwise = tailOf b

-- | Check arguments and read input accordingly.
--
-- If the '-i' flag is present, read from a file specified in the arguments.
-- Otherwise, read input line by line from standard input until EOF.
checkArgs :: IO [String]
checkArgs = do
  args <- getArgs
  if checkFlag args
    then do
      let fileName = tailOf args
      content <- readFile fileName
      return (lines content)
    else whilegetline

-- | Read lines from standard input until EOF is reached.
whilegetline :: IO [String]
whilegetline = do
  isClosed <- isEOF
  if isClosed
    then return []
    else do
      line <- getLine
      rest <- whilegetline
      return (line : rest)

-- | Insert spaces before opening parentheses in a string.
checkparenthese :: String -> String
checkparenthese [] = []
checkparenthese (a:b)
    | a == '(' = ' ' : a : checkparenthese b
    | otherwise = a : checkparenthese b

-- | Convert a list of strings into a single space-separated string,
-- ensuring proper spacing for parentheses.
litostr :: [String] -> String
litostr [] = ""
litostr (a:b) = checkparenthese a ++ " " ++ litostr b

-- | Check if a string has any non-space characters.
checkNotEnd :: String -> Bool
checkNotEnd [] = False
checkNotEnd (a:b)
    | a /= ' ' = True
    | otherwise = checkNotEnd b

-- | Check if parentheses are balanced in the input string,
-- starting from a given nesting level.
checkAllString :: String -> Int -> Bool
checkAllString [] _ = False
checkAllString (a:b) i
    | a == ')' && i == 1 = checkNotEnd b
    | a == ')' = checkAllString b (i - 1)
    | a == '(' = checkAllString b (i + 1)
    | otherwise = checkAllString b i

-- | Add enclosing parentheses to the input string if they are unbalanced.
needParenthese :: String -> String
needParenthese [] = []
needParenthese a
  | checkAllString a 0 = "(" ++ a ++ ")"
  | otherwise = a
