-- | Data structures for S-Expressions and Abstract Syntax Trees (ASTs).
-- This module defines:
-- 1. The `SExpr` type for representing Lisp-style S-expressions.
-- 2. The `AST` type for representing parsed and processed structures.

module Structure (SExpr(..), AST(..)) where

-- | Represents a Lisp-style S-expression.
-- An S-expression can either be:
-- - `Atom`: A single atomic value represented as a string.
-- - `List`: A list of other S-expressions.
data SExpr =
    Atom String    -- ^ An atomic value (e.g., a symbol or a number).
  | List [SExpr]   -- ^ A list of S-expressions.
  deriving (Eq)

-- | Custom `Show` instance for `SExpr`.
-- Converts an `SExpr` into a human-readable string representation.
instance Show SExpr where
    show (Atom str) = str
    show (List xs)  = "(" ++ unwords (map show xs) ++ ")"

-- | Represents an Abstract Syntax Tree (AST).
-- The AST supports:
-- - `SInt`: Integer values.
-- - `SSymbol`: Symbolic values represented as strings.
-- - `SList`: A list of AST nodes.
-- - `SBool`: Boolean values.
data AST =
    SInt Int        -- ^ Integer values.
  | SSymbol String  -- ^ Symbolic values.
  | SList [AST]     -- ^ A list of AST nodes.
  | SBool Bool      -- ^ Boolean values (`True` or `False`).

-- | Custom `Eq` instance for `AST`.
-- Check for each constructors if there are equal
instance Eq AST where
  SInt x == SInt y = x == y
  SSymbol x == SSymbol y = x == y
  SBool x == SBool y = x == y
  SList x == SList y = x == y
  _ == _ = False

-- | Custom `Show` instance for `AST`.
-- Converts an `AST` into a human-readable string representation.
instance Show AST where
  show (SInt i) = "int : " ++ show i
  show (SSymbol str) = "str : " ++ str
  show (SBool b) = if b then "bool : #t" else "bool : #f"
  show (SList xs) = "(" ++ unwords (map show xs) ++ ")"
