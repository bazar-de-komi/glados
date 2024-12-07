-- | Module for defining basic operators for Abstract Syntax Trees (ASTs).
-- This module provides functions to handle operations like equality, comparison, arithmetic, and modulo.

module HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST) where

import Structure (AST(..))

-- | Equality operator.
--
-- Compares two `AST` values for equality:
-- - Integers: Compares their values.
-- - Symbols: Compares their string representations.
-- - Booleans: Compares their truth values.
--
-- ==== Parameters
-- - `Maybe AST`: The first operand.
-- - `Maybe AST`: The second operand.
--
-- ==== Returns
-- A `Maybe AST` containing a `SBool` with the result, or `Nothing` if the inputs are invalid.
eq :: Maybe AST -> Maybe AST -> Maybe AST
eq (Just (SInt x)) (Just (SInt y)) = Just $ SBool (x == y)
eq (Just (SSymbol x)) (Just (SSymbol y)) = Just $ SBool (x == y)
eq (Just (SBool x)) (Just (SBool y)) = Just $ SBool (x == y)
eq _ _ = Nothing

-- | Less-than operator.
--
-- Compares two `AST` values to check if the first is less than the second:
-- - Integers: Compares their values.
-- - Symbols: Compares their string representations lexicographically.
--
-- ==== Parameters
-- - `Maybe AST`: The first operand.
-- - `Maybe AST`: The second operand.
--
-- ==== Returns
-- A `Maybe AST` containing a `SBool` with the result, or `Nothing` if the inputs are invalid.
lt :: Maybe AST -> Maybe AST -> Maybe AST
lt (Just (SInt x)) (Just (SInt y)) = Just $ SBool (x < y)
lt (Just (SSymbol x)) (Just (SSymbol y)) = Just $ SBool (x < y)
lt _ _ = Nothing

-- | Addition operator.
--
-- Adds two `AST` values:
-- - Integers: Returns their sum.
-- - Symbols: Concatenates their string representations.
--
-- ==== Parameters
-- - `Maybe AST`: The first operand.
-- - `Maybe AST`: The second operand.
--
-- ==== Returns
-- A `Maybe AST` containing the result, or `Nothing` if the inputs are invalid.
add :: Maybe AST -> Maybe AST -> Maybe AST
add (Just (SInt x)) (Just (SInt y)) = Just $ SInt (x + y)
add (Just (SSymbol x)) (Just (SSymbol y)) = Just $ SSymbol (x ++ y)
add _ _ = Nothing

-- | Subtraction operator.
--
-- Subtracts the second `AST` value from the first.
--
-- ==== Parameters
-- - `Maybe AST`: The first operand.
-- - `Maybe AST`: The second operand.
--
-- ==== Returns
-- A `Maybe AST` containing the result, or `Nothing` if the inputs are invalid.
subtractAST :: Maybe AST -> Maybe AST -> Maybe AST
subtractAST (Just (SInt x)) (Just (SInt y)) = Just $ SInt (x - y)
subtractAST _ _ = Nothing

-- | Multiplication operator.
--
-- Multiplies two `AST` values.
--
-- ==== Parameters
-- - `Maybe AST`: The first operand.
-- - `Maybe AST`: The second operand.
--
-- ==== Returns
-- A `Maybe AST` containing the result, or `Nothing` if the inputs are invalid.
multiply :: Maybe AST -> Maybe AST -> Maybe AST
multiply (Just (SInt x)) (Just (SInt y)) = Just $ SInt (x * y)
multiply _ _ = Nothing

-- | Division operator.
--
-- Divides the first `AST` value by the second.
--
-- ==== Parameters
-- - `Maybe AST`: The first operand.
-- - `Maybe AST`: The second operand.
--
-- ==== Returns
-- A `Maybe AST` containing the result, or `Nothing` if division by zero or invalid inputs occur.
divAST :: Maybe AST -> Maybe AST -> Maybe AST
divAST (Just (SInt x)) (Just (SInt y))
    | y == 0 = Nothing
    | otherwise = Just $ SInt (x `div` y)
divAST _ _ = Nothing

-- | Modulo operator.
--
-- Computes the remainder of the division of the first `AST` value by the second.
--
-- ==== Parameters
-- - `Maybe AST`: The first operand.
-- - `Maybe AST`: The second operand.
--
-- ==== Returns
-- A `Maybe AST` containing the result, or `Nothing` if division by zero or invalid inputs occur.
modAST :: Maybe AST -> Maybe AST -> Maybe AST
modAST (Just (SInt x)) (Just (SInt y))
    | y == 0 = Nothing
    | otherwise = Just $ SInt (x `mod` y)
modAST _ _ = Nothing
