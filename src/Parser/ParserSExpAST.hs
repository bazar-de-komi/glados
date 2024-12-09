-- | A parser for converting S-expressions into Abstract Syntax Trees (ASTs).
-- This module provides functions to:
-- 1. Identify the type of an S-expression (integer, boolean, or symbol).
-- 2. Convert `SExpr` into `AST`.

module Parser.ParserSExpAST (parseAST, isInt, isBool, finBool, noMaybeParseAST) where

import Structure (SExpr(..), AST(..))
import Data.Maybe (mapMaybe)

-- | Checks if a string represents an integer.
--
-- A valid integer is a sequence of digits (0-9).
isInt :: String -> Bool
isInt [] = True
isInt (a:b)
    | a `elem` ['0'..'9'] = isInt b
    | otherwise = False

-- | Checks if a string represents a boolean value.
--
-- A valid boolean starts with `#` followed by `t` (true) or `f` (false).
isBool :: String -> Bool
isBool (a:b:_) = a == '#' && (b == 'f' || b == 't')
isBool _ = False

-- | Determines the boolean value from a boolean string.
--
-- If the second character is `t`, it returns `True`. Otherwise, `False`.
finBool :: String -> Bool
finBool (_:b:_) = b == 't'
finBool _ = False

-- | Checks if a string represents an negative integer.
--
-- A valid negative integer is a sequence of digits (0-9) with a '-' behind.
isNegInt :: String -> Bool
isNegInt (a:b)
    | a == '-' && isInt b && b /= [] = True
    | otherwise = False
isNegInt _ = False

-- | Converts a single `SExpr` into `AST` without `Maybe`.
--
-- This function is used to process individual elements of an S-expression.
noMaybeParseAST :: SExpr -> Maybe AST
noMaybeParseAST (List a) = Just . SList $ mapMaybe noMaybeParseAST a
noMaybeParseAST (Atom a)
    | isInt a = Just (SInt (read a))
    | isNegInt a = Just (SInt (read a))
    | isBool a = Just (SBool (finBool a))
    | otherwise = Just (SSymbol a)

-- | Parses an `SExpr` into an `AST`.
--
-- The function handles both atomic and list expressions.
--
-- ==== Parameters
-- - `Maybe SExpr`: The input `SExpr`, potentially `Nothing` for invalid input.
--
-- ==== Returns
-- A `Maybe AST` representing the parsed structure, or `Nothing` if parsing fails.
parseAST :: Maybe SExpr -> Maybe AST
parseAST Nothing = Nothing
parseAST (Just (List a)) = Just . SList $ mapMaybe noMaybeParseAST a
parseAST (Just (Atom a))
    | isInt a = Just (SInt (read a))
    | isBool a = Just (SBool (finBool a))
    | otherwise = Just (SSymbol a)
