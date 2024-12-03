module Parser_LISP_SE.Parserlispsexp (parseSExpr) where

import StructureSE.StructureSE (SExpr(..))
import Data.Maybe (mapMaybe)

takefstlist::String -> Char-> Int -> String
takefstlist "" _ _ = []
takefstlist (a:b) c i
    | a == c && c == ')' && i == 0 = [a]
    | a == c && c == ')' = (a : (takefstlist b c (i - 1)))
    | a == c && c == ' ' = []
    | a == '(' && c == ')' = (a : (takefstlist b c (i + 1)))
    | otherwise = (a : (takefstlist b c i))

removefstlist::String -> Char -> Int -> String
removefstlist "" _ _ = []
removefstlist (a:b) c i
    | a == c && c == ')' && i == 0 = b
    | a == c && c == ')' = removefstlist b c (i - 1)
    | a == c && c == ' ' = b
    | a == '(' && c == ')' = removefstlist b c (i + 1)
    | otherwise = removefstlist b c i

splitWords::String -> [String]
splitWords "" = []
splitWords (a:b)
    | '(' == a = (a : (takefstlist (b) ')' 0)) : (splitWords(removefstlist (b) ')' 0))
    | ' ' == a = splitWords b
    | otherwise = (a : (takefstlist (b) ' ' 0)) : (splitWords(removefstlist (b) ' ' 0))

parseSExpr :: String -> Maybe SExpr
parseSExpr input =
  case unwords (words input) of
    '(' : xs | last xs == ')' -> parseList (init xs)
    '(' : _ -> Nothing
    x -> Just $ Atom x
  where
    parseList xs = Just . List $ mapMaybe parseSExpr (splitWords xs)
