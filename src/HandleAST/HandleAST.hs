-- | Module for handling and evaluating Abstract Syntax Trees (ASTs).
-- This module provides functions to process an `AST`, evaluate it, and return the result.

module HandleAST.HandleAST (handleAST, returnValueAST) where

import Structure (AST(..))
import HandleAST.GetValue (getValue, getWithDefine)
import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)
import HandleAST.HandleFunctions (handleFunctions)

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
returnValueAST inast (SSymbol a) = 
    case getValue inast (SSymbol a) of
        Just value -> Just value
        _ -> Nothing
returnValueAST inast (SList (SList (SSymbol "lambda" : body) : values)) =
    handleFunctions inast (SList body) (SList values)
returnValueAST inast (SList (SList (SSymbol "define" : _) : a)) =
    returnValueAST inast (SList a)
returnValueAST inast (SList (SSymbol a : xs)) =
    case a of
        "+" -> add (returnValueAST inast (head xs)) (returnValueAST inast (head (tail xs)))
        "div" -> divAST (returnValueAST inast (head xs)) (returnValueAST inast (head (tail xs)))
        "*" -> multiply (returnValueAST inast (head xs)) (returnValueAST inast (head (tail xs)))
        "-" -> subtractAST (returnValueAST inast (head xs)) (returnValueAST inast (head (tail xs)))
        "<" -> lt (returnValueAST inast (head xs)) (returnValueAST inast (head (tail xs)))
        ">" -> lt (returnValueAST inast (head (tail xs))) (returnValueAST inast (head xs))
        "eq?" -> eq (returnValueAST inast (head xs)) (returnValueAST inast (head (tail xs)))
        "mod" -> modAST (returnValueAST inast (head xs)) (returnValueAST inast (head (tail xs)))
        _ -> case getWithDefine inast (SSymbol a) of
                Just body -> handleFunctions inast body (SList xs)
                Nothing   -> Nothing
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
