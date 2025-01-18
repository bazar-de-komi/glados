-- | Parser for processing a string into S-Expressions (SExpr).
--
-- This module provides a collection of parsing functions for handling a custom
-- Kleftis syntax. It includes parsers for individual atoms, structured expressions,
-- and complete programs, all built using Megaparsec.

module Parser.ParserCompilVM(pProgramInst, pGroupedExprInst, parseStringInst, parsIntInst, noSpaceInst) where

import Structure (Instruction(..), BinaryOperator(..), BinaryComparator(..), Value(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Type alias for the custom parser using Megaparsec.
type MyParser = Parsec Void String

-- | Consumes spaces and tabs only, skipping block and line comments.
noSpaceInst :: MyParser ()
noSpaceInst = L.space (void $ some (oneOf " \t")) (L.skipBlockComment "<--" "-->") (L.skipLineComment "<-")

-- | Skips unnecessary single newlines.
noUselessNInst :: MyParser ()
noUselessNInst = void $ optional (try (char '\n' <* notFollowedBy (char '\n')))

-- | Skips to the next line by consuming trailing spaces and comments.
nextLineInst :: MyParser ()
nextLineInst = noSpaceInst <* noUselessNInst <* noSpaceInst

-- | Parses a signed integer.
parsIntInst :: MyParser Int
parsIntInst = try $ L.signed (return ()) L.decimal

-- | Parses a signed floating-point number.
parsFloatInst :: MyParser Float
parsFloatInst = try $ L.signed (return ()) L.float <* notFollowedBy letterChar

-- | Parses a string delimited by `#@` and ending with `#`.
parseStringInst :: MyParser String
parseStringInst = string "\"" *> manyTill anySingle (lookAhead (char '\"')) <* char '\"' <* noSpaceInst

-- | Parses a string delimited by `#@` and ending with `#`.
parseCharInst :: MyParser Char
parseCharInst = string "\'" *> anySingle <* char '\''

-- | Parses a boolean value (`True` or `False`).
parseBoolInst :: MyParser Bool
parseBoolInst = try $ choice
  [ True <$ string "True" <* lookAhead (char '\n')
  , False <$ string "False" <* lookAhead (char '\n')
  ]

-- | Parses a single S-expression.
parseVAlue :: MyParser Value
parseVAlue = choice
  [ VFloat <$> parsFloatInst
  , VInt <$> parsIntInst
  , VString <$> parseStringInst
  , VBool <$> parseBoolInst
  , VChar <$> parseCharInst
  ] <* noSpaceInst

-- | Parser for a list of one or more S-expressions.
--   Example: `a b c` will be parsed as `List [a, b, c]`.
parseStoreConst :: MyParser Instruction
parseStoreConst =
  string "STORE_CONST" <* noSpaceInst *>
  (STORE_CONST <$> parseVAlue)

-- | Parses a `STORE_VAR` instruction with its associated variable name.
parseStoreVar :: MyParser Instruction
parseStoreVar =
  string("STORE_VAR") <* noSpaceInst *>
  (STORE_VAR <$> parseStringInst)

-- | Parses a `LOAD_VAR` instruction with its associated variable name.
parseLoadVar :: MyParser Instruction
parseLoadVar =
  string("LOAD_VAR") <* noSpaceInst *>
  (LOAD_VAR <$> parseStringInst)

-- | Parses a `JUMP` instruction with its target label.
parseJump :: MyParser Instruction
parseJump =
  string("JUMP") <* noSpaceInst *>
  (JUMP <$> parseStringInst)

-- | Parses a `JUMP_IF_FALSE` instruction with its target label.
parseJumpIfFalse :: MyParser Instruction
parseJumpIfFalse =
  string("JUMP_IF_FALSE") <* noSpaceInst *>
  (JUMP_IF_FALSE <$> parseStringInst)

-- | Parses a `LABEL` instruction with its label name.
parseLabel :: MyParser Instruction
parseLabel =
  string("LABEL") <* noSpaceInst *>
  (LABEL <$> parseStringInst)

-- | Parses a `LABEL_FUNC` instruction with its label name.
parseLabelFunc :: MyParser Instruction
parseLabelFunc =
  string("LABEL_FUNC") <* noSpaceInst *>
  (LABEL_FUNC <$> parseStringInst)

-- | Parses a `LABEL_FUNC_END` instruction.
parseLabelFuncEnd :: MyParser Instruction
parseLabelFuncEnd =
  string("LABEL_FUNC_END") <* noSpaceInst *>
  (LABEL_FUNC_END <$> parseStringInst)

-- | Parses a `CALL` instruction with its target function label.
parseCall :: MyParser Instruction
parseCall =
  string("CALL") <* noSpaceInst *>
  (CALL <$> parseStringInst)

-- | Parses a `RETURN` instruction.
parseReturn :: MyParser Instruction
parseReturn = RETURN <$ string("RETURN") <* noSpaceInst

-- | Parses a `HALT` instruction.
parseHalt :: MyParser Instruction
parseHalt = HALT <$ string("HALT") <* noSpaceInst

-- | Parses a binary operator.
parseOperator :: MyParser BinaryOperator
parseOperator = string("OPERATOR") <* noSpaceInst *> choice
  [ ADD <$ string("ADD")
  , SUBTRACT <$ string("SUBTRACT")
  , MULTIPLY <$ string("MULTIPLY")
  , DIVIDE <$ string("DIVIDE")
  , MODULO <$ string("MODULO")
  ] <* noSpaceInst

-- | Parses a binary comparator.
parseComparator :: MyParser BinaryComparator
parseComparator = string("COMPARATOR") <* noSpaceInst *> choice
  [ COMPARE_GT <$ string("COMPARE_GT")
  , COMPARE_LT <$ string("COMPARE_LT")
  , COMPARE_EQ <$ string("COMPARE_EQ")
  , COMPARE_NE <$ string("COMPARE_NE")
  , COMPARE_GE <$ string("COMPARE_GE")
  , COMPARE_LE <$ string("COMPARE_LE")
  ] <* noSpaceInst

-- | Parser for a non-empty group.
--   Consumes leading spaces or comments before attempting to parse either a list or an empty group.
parseInstruction :: MyParser Instruction
parseInstruction = choice
  [ OPERATOR <$> parseOperator
  , COMPARATOR <$> parseComparator
  , parseStoreConst
  , parseStoreVar
  , parseLoadVar
  , parseJumpIfFalse
  , parseJump
  , parseLabelFuncEnd
  , parseLabelFunc
  , parseLabel
  , parseCall
  , parseReturn
  , parseHalt
  ]

-- | Parses groups of S-expressions separated by double newlines (`\n\n`).
pGroupedExprInst :: MyParser [Instruction]
pGroupedExprInst = sepBy1 (parseInstruction) (string "\n") <* nextLineInst

-- | Parses a complete program of S-expressions until EOF.
pProgramInst :: MyParser [Instruction]
pProgramInst = pGroupedExprInst <* eof
