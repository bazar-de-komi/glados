-- | Module for handling and evaluating Abstract Syntax Trees (ASTs).
-- This module provides functions to process an `AST`, evaluate it, and return the result.

module HandleAST.HandleAST (handleAST) where

import HandleAST.GetValue (getValue)
import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)
import Structure (AST(..))

-- | Evaluate an `AST` and return its value.
--
-- The function processes different types of `AST` nodes, including:
-- - Integers and Booleans: Returns their value directly.
-- - Symbols: Resolves their value using the `getValue` function.
-- - Lists: Evaluates expressions based on operators like `+`, `div`, `*`, `-`, `<`, `>`, `eq?`, and `mod`.
--
-- ==== Parameters
-- - `AST`: The input `AST` to evaluate.
-- - `AST`: The `AST` representing the environment (for resolving symbols).
--
-- ==== Returns
-- A `Maybe AST` containing the result of the evaluation, or `Nothing` if evaluation fails.
returnValueAST :: AST -> AST -> Maybe AST
returnValueAST _ (SInt a) = Just (SInt a)
returnValueAST _ (SBool a) = Just (SBool a)
returnValueAST inast (SSymbol a)
    | getValue inast (SSymbol a) == Nothing = Just (SSymbol a)
    | otherwise = getValue inast (SSymbol a)
returnValueAST inast (SList (SList (SSymbol "define" : _) : a)) = returnValueAST inast (SList a)
returnValueAST inast (SList (SList a : _)) = returnValueAST inast (SList a)
returnValueAST inast (SList (SSymbol a : b : c : _))
    | a == "+" = add (returnValueAST inast b) (returnValueAST inast c)
    | a == "div" = divAST (returnValueAST inast b) (returnValueAST inast c)
    | a == "*" = multiply (returnValueAST inast b) (returnValueAST inast c)
    | a == "-" = subtractAST (returnValueAST inast b) (returnValueAST inast c)
    | a == "<" = lt (returnValueAST inast b) (returnValueAST inast c)
    | a == ">" = lt (returnValueAST inast c) (returnValueAST inast b)
    | a == "eq?" = eq (returnValueAST inast b) (returnValueAST inast c)
    | a == "mod" = modAST (returnValueAST inast b) (returnValueAST inast c)
    | otherwise = Nothing
returnValueAST _ (SList (_ : _ : _)) = Nothing
returnValueAST inast (SList (a : _)) = returnValueAST inast a
returnValueAST _ _ = Nothing

-- | Handles and evaluates the given `AST`.
--
-- If evaluation is successful, the result is printed. Otherwise, an error message is displayed.
--
-- ==== Parameters
-- - `Maybe AST`: The input `AST` to handle. If `Nothing`, an error message is displayed.
handleAST :: Maybe AST -> IO ()
handleAST Nothing = putStrLn "ERROR: Failed to parse check your Lisp expression!"
handleAST (Just a)
    | returnValueAST a a /= Nothing = print (returnValueAST a a)
    | otherwise = putStrLn "ERROR: Failed no return value!"
