-- | A parser for Lisp-style S-expressions.
-- This module provides functions to:
-- 1. Parse a string into an `SExpr` data structure.
-- 2. Split strings into meaningful components like atoms and lists.

module Parser.ParserLispSExp (parseSExpr, takefstlist, removefstlist, splitWords) where

import Structure (SExpr(..))
import Data.Maybe (mapMaybe)

-- | Extracts the first list (or group) from a string.
--
-- The function starts parsing a group when encountering an opening character
-- and stops when encountering a closing character at the correct nesting level.
--
-- ==== Parameters
-- - `String`: The input string.
-- - `Char`: The closing character to match.
-- - `Int`: The current nesting level.
--
-- ==== Returns
-- A substring representing the first list (or group).
takefstlist :: String -> Char -> Int -> String
takefstlist "" _ _ = []
takefstlist (a:b) c i
    | a == c && c == ')' && i == 0 = [a]
    | a == c && c == ')' = (a : (takefstlist b c (i - 1)))
    | a == c && c == ' ' = []
    | a == '(' && c == ')' = (a : (takefstlist b c (i + 1)))
    | otherwise = (a : (takefstlist b c i))

-- | Removes the first list (or group) from a string.
--
-- This function skips over the first group defined by matching parentheses
-- or spaces and returns the remainder of the string.
removefstlist :: String -> Char -> Int -> String
removefstlist "" _ _ = []
removefstlist (a:b) c i
    | a == c && c == ')' && i == 0 = b
    | a == c && c == ')' = removefstlist b c (i - 1)
    | a == c && c == ' ' = b
    | a == '(' && c == ')' = removefstlist b c (i + 1)
    | otherwise = removefstlist b c i

-- | Splits a string into meaningful components (atoms and groups).
--
-- The function separates atoms and groups by spaces or parentheses.
splitWords :: String -> [String]
splitWords "" = []
splitWords (a:b)
    | '(' == a = (a : (takefstlist (b) ')' 0)) : (splitWords(removefstlist (b) ')' 0))
    | ' ' == a = splitWords b
    | otherwise = (a : (takefstlist (b) ' ' 0)) : (splitWords(removefstlist (b) ' ' 0))

-- | Parses a string into an `SExpr`.
--
-- The input string is assumed to follow Lisp syntax, with balanced parentheses.
-- Atoms are treated as strings, and lists are recursively parsed.
--
-- ==== Parameters
-- - `String`: The input string representing a Lisp expression.
--
-- ==== Returns
-- A `Maybe SExpr` containing the parsed result, or `Nothing` if the input is invalid.
parseSExpr :: String -> Maybe SExpr
parseSExpr input =
  case unwords (words input) of
    '(' : xs | last xs == ')' -> parseList (init xs)
    '(' : _ -> Nothing
    x -> Just $ Atom x
  where
    parseList xs = Just . List $ mapMaybe parseSExpr (splitWords xs)
