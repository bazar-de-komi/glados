-- | Utility functions for handling arguments, input, and parentheses.
-- This module provides functions to:
-- 1. Read command-line arguments or standard input.
-- 2. Convert input into a single string.
-- 3. Handle parentheses in Lisp-like expressions.

module Lib (checkArgs, giveFileName, litostr, needParenthese, whilegetline, checkparenthese, checkNotEnd, checkAllString, backToFile) where

import System.IO

giveFileName :: [String] -> String -> String
giveFileName [] _ = ""
giveFileName ("-c" : ('-':_) : _) "-c" = ""
giveFileName ("-c" : a : _) "-c" = a
giveFileName ("-i" : ('-':_) : _) "-i" = ""
giveFileName ("-i" : a : _) "-i" = a
giveFileName (_:b) a = giveFileName b a

-- | Check arguments and read input accordingly.
--
-- If the '-i' flag is present, read from a file specified in the arguments.
-- Otherwise, read input line by line from standard input until EOF.
checkArgs :: [String] -> IO [String]
checkArgs args = do
  if "-i" `elem` args
    then do
      content <- readFile (giveFileName args "-i")
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
    | a == '(' = ' ' : a : ' ' : checkparenthese b
    | a == ')' = ' ' : a : ' ' : checkparenthese b
    | a == '{' = ' ' : a : ' ' : checkparenthese b
    | a == '}' = ' ' : a : ' ' : checkparenthese b
    | a == '[' = ' ' : a : ' ' : checkparenthese b
    | a == ']' = ' ' : a : ' ' : checkparenthese b
    | otherwise = a : checkparenthese b

-- | Convert a list of strings into a single space-separated string,
-- ensuring proper spacing for parentheses.
litostr :: [String] -> String
litostr [] = ""
litostr (a:[]) = a
litostr (a:b) = checkparenthese a ++ "\n" ++ litostr b

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

-- | return File in one string with \n
backToFile :: [String] -> String
backToFile [] = ""
backToFile (a:b) = a ++ "\n" ++ (backToFile b)
