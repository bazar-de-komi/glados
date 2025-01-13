-- | A parser module for processing a string into S-expressions.
module Parser.ParserLispSExp (pProgram, pGroupedExpr, pNonEmptyGroup, pList, pExpr, parseAtom, parseString, parsInt, noSpace, noSpaceForNew) where

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

nextLine :: MyParser ()
nextLine = noSpace <* noUselessN <* noSpace

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

parseBool :: MyParser Bool
parseBool = try $ choice
  [ True <$ string "True" <* lookAhead (char ' ')
  , False <$ string "False" <* lookAhead (char ' ')
  ]

parseBasicFunc :: MyParser String
parseBasicFunc = some (oneOf ":|/+<>-*=")

parsChar :: MyParser Char
parsChar = char '#' *> anySingle

parseAtom :: MyParser String
parseAtom = some (noneOf (" \t\n\r{([<:|/+-*=>])}"))

parseParam :: MyParser SExpr
parseParam =
  Param <$> (char '[' <* noSpace *> some pExpr <* char ']' <* nextLine)

parseList :: MyParser SExpr
parseList =
  List <$> (char '(' <* nextLine *> some pExpr <* char ')' <* nextLine)

parseSEList :: MyParser SExpr
parseSEList =
  SEList <$> (char '{' <* nextLine *> some pExpr <* char '}' <* nextLine)

parseReturn :: MyParser SExpr
parseReturn = List <$> (
    try (choice [string "résult" <* lookAhead (char ' '), string "result" <* lookAhead (char ' ')])
    *> noSpace *> some pExpr <* nextLine
  )

parseIf :: MyParser SExpr
parseIf = do
  _ <- try $ string "si" <* lookAhead (char ' ') <* noSpace
  cond <- parseParam
  content <- parseList
  contentelse <- option (List []) $ (string "sinon" <* nextLine *> parseList)
  return $ SEIf cond content contentelse

parseLoop :: MyParser SExpr
parseLoop = do
  _ <- try $ string "tankeu" <* lookAhead (char ' ') <* noSpace
  cond <- parseParam
  content <- parseList
  return $ SELoop cond content

parseFor :: MyParser SExpr
parseFor = do
  _ <- try $ string "pour" <* lookAhead (char ' ') <* noSpace
  initc <- parseList
  cond <- parseParam
  update <- parseList
  content <- parseList
  return $ SEFor initc cond update content

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
  , Return <$> parseReturn
  , parseList
  , parseLoop
  , parseFor
  , parseIf
  , parseSEList
  , Atom  <$> parseAtom
  ] <* noSpace

-- | Parser for a list of one or more S-expressions.
--   Example: `a b c` will be parsed as `List [a, b, c]`.
pList :: MyParser SExpr
pList = List <$> some pExpr

-- | Parser for a non-empty group.
--   Consumes leading spaces or comments before attempting to parse either a list or an empty group.
pNonEmptyGroup :: MyParser SExpr
pNonEmptyGroup = noSpaceForNew *> pList

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
