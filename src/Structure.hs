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
    Atom String
  | SEInt Int
  | SEChar Char
  | SEString String
  | SEFloat Float
  | Boolean String
  | Type String
  | BasicFunc String
  | Param [SExpr]
  | SEList [SExpr]
  | List [SExpr]
  deriving (Eq, Show)

-- | Represents an Abstract Syntax Tree (AST) for a Lisp-like programming language.
--
-- The AST supports various constructs, including:
--
-- - `SBool`: Represents a generic boolean value (`True` or `False`).
-- - `SFloat`: Represents a floating-point number.
-- - `SInt`: Represents an integer value.
-- - `SString`: Represents a string value.
-- - `SOperation`: Represents the name of an operation (e.g., `+`, `-`, `*`).
-- - `SCall`: Represents a function call with the function name, parameters, and body.
-- - `SDefine`: Represents the definition of a variable or function that takes no parameters.
-- - `SLoop`: Represents a `while` loop, including its condition and body.
-- - `SFor`: Represents a `for` loop with initialization, condition, update, and body.
-- - `SIf`: Represents a conditional `if` expression with a condition, a `then` branch, and an `else` branch.
-- - `SList`: Represents a list containing multiple AST nodes.
--
-- Each constructor is designed to model a specific component of the language's syntax and semantics.

data AST =
    SBool Bool            -- ^ A generic boolean value.
  | SFloat Float          -- ^ A generic float value.
  | SInt Int              -- ^ A generic integer value.
  | SChar Char              -- ^ A generic integer value.
  | SType String             -- ^ A generic integer value.
  | SString String        -- ^ A generic string value.
  | SVariable String
  | SOperation String     -- ^ Represents the name of an operation.
  | SCall String AST AST      -- ^ Represents a function call with the function name, parameter(s), and body.
  | SFunc String AST AST AST
  | SDefine String AST AST    -- ^ Defines a function or variable that does not take any parameters.
  | SLoop AST AST         -- ^ Represents a `while` loop with a condition and a body.
  | SFor AST AST AST AST  -- ^ Represents a `for` loop with initialization, condition, update, and body.
  | SIf AST AST AST       -- ^ Represents a conditional `if` with a condition, a `then` branch, and an `else` branch.
  | SList [AST]           -- ^ Represents a list of multiple AST nodes.
  deriving (Eq, Show)
