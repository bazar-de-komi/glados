-- | Parser for processing a string into S-Expressions (SExpr).
--
-- This module provides a collection of parsing functions for handling a custom
-- Kleftis syntax. It includes parsers for individual atoms, structured expressions,
-- and complete programs, all built using Megaparsec.

module Parser.ParserKleftisSExp (pProgram, pGroupedExpr, pNonEmptyGroup, pExpr, parseAtom, parseString, parsInt, noSpace, noSpaceForNew) where

import Structure (SExpr(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Type alias for the custom parser using Megaparsec.
type MyParser = Parsec Void String

-- | Consumes spaces, tabs, and single newlines while skipping comments.
-- Handles block comments (`<-- ... -->`) and line comments (`<- ...`).
noSpaceForNew :: MyParser ()
noSpaceForNew = L.space space1 (L.skipBlockComment "<--" "-->") (L.skipLineComment "<-")

-- | Consumes spaces and tabs only, skipping block and line comments.
noSpace :: MyParser ()
noSpace = L.space (void $ some (oneOf " \t")) (L.skipBlockComment "<--" "-->") (L.skipLineComment "<-")

-- | Skips unnecessary single newlines.
noUselessN :: MyParser ()
noUselessN = void $ optional (try (char '\n' <* notFollowedBy (char '\n')))

-- | Skips to the next line by consuming trailing spaces and comments.
nextLine :: MyParser ()
nextLine = noSpace <* noUselessN <* noSpace

-- | Parses a signed integer.
parsInt :: MyParser Int
parsInt = try $ L.signed (return ()) L.decimal

-- | Parses a signed floating-point number.
parsFloat :: MyParser Float
parsFloat = try $ L.signed (return ()) L.float <* notFollowedBy letterChar

-- | Parses a string delimited by `#@` and ending with `#`.
parseString :: MyParser String
parseString = string "#@" *> manyTill anySingle (lookAhead (char '#')) <* char '#'

-- | Parses a type annotation (e.g., `int`, `float`, `bool`).
parseType :: MyParser String
parseType = try $ choice
  [ string "int" <* lookAhead (char ' ')
  , string "str" <* lookAhead (char ' ')
  , string "float" <* lookAhead (char ' ')
  , string "bool" <* lookAhead (char ' ')
  , string "char" <* lookAhead (char ' ')
  ]

-- | Parses a boolean value (`True` or `False`).
parseBool :: MyParser Bool
parseBool = try $ choice
  [ True <$ string "True" <* lookAhead (char ' ')
  , False <$ string "False" <* lookAhead (char ' ')
  ]

-- | Parses a basic function or operation (e.g., `+`, `-`, `*`, `=`).
parseBasicFunc :: MyParser String
parseBasicFunc = some (oneOf ":|/+<>-*=")

-- | Parses a character prefixed with `#`.
parsChar :: MyParser Char
parsChar = char '#' *> anySingle

-- | Parses an atomic value (e.g., symbols, variables).
parseAtom :: MyParser String
parseAtom = some (noneOf (" \t\n\r{([<:|/+-*=>])}"))

-- | Parses a list of parameters enclosed in `[]`.
parseParam :: MyParser SExpr
parseParam = Param <$> (char '[' <* noSpace *> some pExpr <* char ']' <* nextLine)

-- | Parses a list of expressions enclosed in `()`.
parseList :: MyParser SExpr
parseList = List <$> (char '(' <* nextLine *> some pExpr <* char ')' <* nextLine)

-- | Parses a list of expressions enclosed in `{}`.
parseSEList :: MyParser SExpr
parseSEList = SEList <$> (char '{' <* nextLine *> some pExpr <* char '}' <* nextLine)

-- | Parses a return expression starting with `résult` or `result`.
parseReturn :: MyParser SExpr
parseReturn = List <$> (
  try (choice [string "résult" <* lookAhead (char ' '), string "result" <* lookAhead (char ' ')])
  *> noSpace *> some pExpr <* nextLine
  )

-- | Parses a conditional `if` expression starting with `si`.
parseIf :: MyParser SExpr
parseIf = SEIf <$> (try (string "si" <* lookAhead (char ' ') <* noSpace) *> parseParam)
               <*> parseList
               <*> option (List []) (string "sinon" <* nextLine *> parseList)

-- | Parses a `while` loop expression starting with `tankeu`.
parseLoop :: MyParser SExpr
parseLoop = SELoop <$> (try (string "tankeu" <* lookAhead (char ' ') <* noSpace) *> parseParam)
                   <*> parseList

-- | Parses a `for` loop expression starting with `pour`.
parseFor :: MyParser SExpr
parseFor = SEFor <$> (try (string "pour" <* lookAhead (char ' ') <* noSpace) *> parseList)
                 <*> parseParam
                 <*> parseList
                 <*> parseList

-- | Parses a single S-expression.
pExpr :: MyParser SExpr
pExpr = choice
  [ SEFloat <$> parsFloat
  , SEInt <$> parsInt
  , SEString <$> parseString
  , SEChar <$> parsChar
  , Type <$> parseType
  , BasicFunc <$> parseBasicFunc
  , Boolean <$> parseBool
  , parseParam
  , Return <$> parseReturn
  , parseList
  , parseLoop
  , parseFor
  , parseIf
  , parseSEList
  , Atom <$> parseAtom
  ] <* noSpace

-- | Parser for a list of one or more S-expressions.
--   Example: `a b c` will be parsed as `List [a, b, c]`.


-- | Parser for a non-empty group.
--   Consumes leading spaces or comments before attempting to parse either a list or an empty group.
pNonEmptyGroup :: MyParser SExpr
pNonEmptyGroup = noSpaceForNew *> (List <$> some pExpr)

-- | Parses groups of S-expressions separated by double newlines (`\n\n`).
pGroupedExpr :: MyParser SExpr
pGroupedExpr = List <$> (
  sepBy1 pNonEmptyGroup (string "\n\n") <* skipMany (char '\n')
  )

-- | Parses a complete program of S-expressions until EOF.
pProgram :: MyParser SExpr
pProgram = pGroupedExpr <* eof
