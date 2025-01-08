-- | A parser for Lisp-style S-expressions.
-- This module provides functions to:
-- 1. Parse a string into an `SExpr` data structure.
-- 2. Split strings into meaningful components like atoms and lists.

module Parser.ParserLispSExp (parseSExpr, takefstlist, removefstlist, splitWords, pProgram) where

import Structure (SExpr(..))
import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Char (isSpace)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L

import Debug.Trace (trace)

data MyError =
    LexicalError String
  | SyntaxError String
  | SemanticError String
  deriving (Eq, Ord, Show)

type MyParser = Parsec Void String

noSpaceForNew :: MyParser ()
noSpaceForNew = L.space
        space1                     -- consomme espace, '\n' et tab
        (L.skipBlockComment "<--" "-->") -- exemple : commentaire multiligne
        (L.skipLineComment "<-")         -- exemple : commentaire monoligne

noSpace :: MyParser ()
noSpace = L.space
        (void $ some (oneOf " \t"))                     -- consomme espace et tab
        (L.skipBlockComment "<--" "-->") -- exemple : commentaire multiligne
        (L.skipLineComment "<-")         -- exemple : commentaire monoligne

clean :: MyParser a -> MyParser a
clean p = p <* noSpace

parsInt :: MyParser Int
parsInt = clean $ L.decimal

parseString :: MyParser String
parseString = clean $ ("#@" <>) <$> (string "#@" *> manyTill anySingle (lookAhead (char '#')) <* char '#')

parseAtom :: MyParser String
parseAtom = clean $ do
  content <- some (noneOf (" \t\n\r")) <* optional (try (char '\n' <* notFollowedBy (char '\n')))
  trace (content ++ "ça reprend ici") $ return ()
  return content
pExpr :: MyParser SExpr
pExpr = clean $ choice
      [ SEInt <$> parsInt
      , Atom  <$> parseString
      , Atom  <$> parseAtom
      ]

pList :: MyParser SExpr
pList = List <$> some pExpr

pEmptyGroup :: MyParser SExpr
pEmptyGroup = return (List [])

pNonEmptyGroup :: MyParser SExpr
pNonEmptyGroup = noSpaceForNew *> choice [pList, pEmptyGroup]

pGroupedExpr :: MyParser SExpr
pGroupedExpr =
    List <$> (sepBy1 pNonEmptyGroup (string "\n\n") <* skipMany (char '\n'))

pProgram :: MyParser SExpr
pProgram = pGroupedExpr <* eof

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
    | a == c && c == '"' = "\""
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
    | a == c && c == '"' = b
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
    | '"' == a = (a : takefstlist (b) '"' 0) : (splitWords(removefstlist (b) '"' 0))
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
    '(' : xs | xs /= [] && last xs == ')' -> parseList (init xs)
    '(' : _ -> Nothing
    '"' : xs | xs /= [] && last xs == '"' -> Just $ Atom (init xs)
    '"' : _ -> Nothing
    x -> Just $ Atom x
  where
    parseList xs = Just . List $ mapMaybe parseSExpr (splitWords xs)
