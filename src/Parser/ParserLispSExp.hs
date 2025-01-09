-- | A parser module for processing a string into S-expressions.
module Parser.ParserLispSExp (pProgram, pGroupedExpr, pNonEmptyGroup, pEmptyGroup, pList, pExpr, parseAtom, parseString, parsInt, noSpace, noSpaceForNew) where

import Structure (SExpr(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Custom error type to represent parsing-related issues.
data MyError =
    LexicalError String    -- ^ Represents a lexical error, such as an invalid character.
  | SyntaxError String     -- ^ Represents a syntax error, such as incorrect structure.
  | SemanticError String   -- ^ Represents a semantic error, such as invalid logic or operations.
  deriving (Eq, Ord, Show)

-- | Type alias for the custom parser using Megaparsec.
--   It uses `Void` for the error type and `String` for the input type.
type MyParser = Parsec Void String

-- | Parser to consume spaces, tabs, and single newlines.
--   This also skips block comments (`<-- ... -->`) and line comments (`<- ...`).
noSpaceForNew :: MyParser ()
noSpaceForNew = L.space
  space1                           -- ^ Consumes spaces, tabs, and single newlines.
  (L.skipBlockComment "<--" "-->") -- ^ Skips block comments of the form `<-- ... -->`.
  (L.skipLineComment "<-")         -- ^ Skips single-line comments starting with `<-`.

-- | Parser to consume spaces and tabs only.
--   It also skips block and line comments.
noSpace :: MyParser ()
noSpace = L.space
  (void $ some (oneOf " \t"))        -- ^ Consumes spaces and tabs only.
  (L.skipBlockComment "<--" "-->")  -- ^ Skips block comments of the form `<-- ... -->`.
  (L.skipLineComment "<-")          -- ^ Skips single-line comments starting with `<-`.

noUselessN :: MyParser ()
noUselessN = void $ optional (try (char '\n' <* notFollowedBy (char '\n')))

parsInt :: MyParser Int
parsInt = try $ L.signed (return ()) L.decimal

parsFloat :: MyParser Float
parsFloat = try $ L.signed (return ()) L.float <* notFollowedBy letterChar

parseString :: MyParser String
parseString = string "#@" *> manyTill anySingle (lookAhead (char '#')) <* char '#'

parseType :: MyParser String
parseType = try $ choice
  [ string "int" <* lookAhead (char ' ')
  , string "str" <* lookAhead (char ' ')
  , string "float" <* lookAhead (char ' ')
  , string "bool" <* lookAhead (char ' ')
  , string "char" <* lookAhead (char ' ')
  ]

parseBasicFunc :: MyParser String
parseBasicFunc = some (oneOf ":|/+-*=")

parseBool :: MyParser String
parseBool = choice
  [ string "True"
  , string "False"
  ]

parseParam :: MyParser SExpr
parseParam = do
  _ <- char '[' <* noSpace
  content <- Param <$> some pExpr
  _ <- char ']' <* noSpace <* noUselessN
  return content

parseList :: MyParser SExpr
parseList = do
  _ <- char '(' <* noSpace
  content <- List <$> some pExpr
  _ <- char ')' <* noSpace <* noUselessN
  return content

parseSEList :: MyParser SExpr
parseSEList = do
  _ <- char '{' <* noSpace
  content <- SEList <$> some pExpr
  _ <- char '}' <* noSpace <* noUselessN
  return content

parsChar :: MyParser Char
parsChar = do
  _ <- char '#'
  content <- anySingle
  return content

parseAtom :: MyParser String
parseAtom = noUselessN *> optional (noSpace) *> some (noneOf (" \t\n\r{([])}"))

pExpr :: MyParser SExpr
pExpr = choice
  [ SEFloat  <$> parsFloat
  , SEInt <$> parsInt
  , SEString  <$> parseString
  , SEChar <$> parsChar
  , Type  <$> parseType
  , BasicFunc  <$> parseBasicFunc
  , Boolean <$> parseBool
  , parseParam
  , parseList
  , parseSEList
  , Atom  <$> parseAtom
  ] <* noSpace

-- | Parser for a list of one or more S-expressions.
--   Example: `a b c` will be parsed as `List [a, b, c]`.
pList :: MyParser SExpr
pList = List <$> some pExpr

-- | Parser for an empty group.
--   Always returns an empty `List []`.
pEmptyGroup :: MyParser SExpr
pEmptyGroup = return (List [])

-- | Parser for a non-empty group.
--   Consumes leading spaces or comments before attempting to parse either a list or an empty group.
pNonEmptyGroup :: MyParser SExpr
pNonEmptyGroup = noSpaceForNew *> choice [pList, pEmptyGroup]

-- | Parser for groups of S-expressions separated by double newlines (`\n\n`).
--   Consumes extra trailing newlines at the end.
--   Example: Two groups `a b\n\nc d` will be parsed as `List [List [a, b], List [c, d]]`.
pGroupedExpr :: MyParser SExpr
pGroupedExpr = List <$> (
  sepBy1 pNonEmptyGroup (string "\n\n") -- ^ Separates groups using double newlines.
  <* skipMany (char '\n')               -- ^ Skips extra trailing newlines after the last group.
  )

-- | Main parser for an entire program.
--   Parses a collection of S-expressions, consuming all input until EOF.
--   Example: A full program with multiple groups will result in a nested `List` structure.
pProgram :: MyParser SExpr
pProgram = pGroupedExpr <* eof -- ^ Ensures all input is consumed and no trailing content exists.
