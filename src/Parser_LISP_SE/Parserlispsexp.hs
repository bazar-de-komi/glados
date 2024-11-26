module Parser_LISP_SE.Parserlispsexp (parseSExpr) where

import StructureSE.StructureSE (SExpr(..))
import Data.Maybe (mapMaybe)

takefstlist::String -> Char -> String
takefstlist "" _ = []
takefstlist (a:b) c
    | a == c && c == ')' = [a]
    | a == c && c == ' ' = []
    | otherwise = (a : (takefstlist b c))

removefstlist::String -> Char -> String
removefstlist "" _ = []
removefstlist (a:b) c
    | a == c = b
    | otherwise = removefstlist b c

splitWords::String -> [String]
splitWords "" = []
splitWords (a:b)
    | '(' == a = (takefstlist (a:b) ')') : (splitWords(removefstlist (a:b) ')'))
    | otherwise = (takefstlist (a:b) ' ') : (splitWords(removefstlist (a:b) ' '))

parseSExpr :: String -> Maybe SExpr
parseSExpr input =
  case unwords (words input) of
    '(' : xs | last xs == ')' -> parseList (init xs)
    '(' : xs -> Nothing
    x -> Just $ Atom x
  where
    parseList xs = Just . List $ mapMaybe parseSExpr (splitWords xs)
