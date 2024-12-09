-- | Module for handling and evaluating Abstract Syntax Trees (ASTs).
-- This module provides functions to process an `AST`, evaluate it, and return the result.

module HandleAST.HandleAST (handleAST, returnValueAST, handleFunctions) where

import Structure (AST(..))
import HandleAST.GetValue (getValue, getWithDefine)
import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)
import HandleAST.FunctionsUtils (bindParameters, substituteBindings)
import HandleAST.ConditionalExpressions (condExpress)

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
returnValueAST inast (SList (SList (SSymbol "lambda" : body) : values)) =
    handleFunctions inast (SList body) (SList values)
returnValueAST inast (SList (SList (SSymbol "define" : _) : a)) = returnValueAST inast (SList a)
returnValueAST inast (SList (SList a : _)) = returnValueAST inast (SList a)
returnValueAST inast (SList (SSymbol a : b : c : d))
    | a == "+" = add (returnValueAST inast b) (returnValueAST inast c)
    | a == "div" = divAST (returnValueAST inast b) (returnValueAST inast c)
    | a == "*" = multiply (returnValueAST inast b) (returnValueAST inast c)
    | a == "-" = subtractAST (returnValueAST inast b) (returnValueAST inast c)
    | a == "<" = lt (returnValueAST inast b) (returnValueAST inast c)
    | a == ">" = lt (returnValueAST inast c) (returnValueAST inast b)
    | a == "eq?" = eq (returnValueAST inast b) (returnValueAST inast c)
    | a == "mod" = modAST (returnValueAST inast b) (returnValueAST inast c)
    | a == "if" = condExpress inast b c d
    | otherwise = case getWithDefine inast (SSymbol a) of
                    Just body -> handleFunctions inast body (SList (b : c : d))
                    Nothing   -> Nothing
returnValueAST _ (SList (_ : _ : _)) = Nothing
returnValueAST inast (SList (a : _)) = returnValueAST inast a
returnValueAST _ _ = Nothing

-- |
-- ==== Parameters
-- - `AST`: The `AST` representing the environment (for resolving symbols)
-- - `AST`: The `AST` representing the bindings of parameters to their values
-- - `AST`: The body of the function to evaluate
--
-- ==== Returns
-- - `Maybe AST`: containing the result of the evaluation
-- - `Nothing`: if evaluation fails
returnValueHandleFunction :: AST -> AST -> AST -> Maybe AST
returnValueHandleFunction env bindings body =
    case substituteBindings bindings body of
        SList substitutedBody -> returnValueAST env (SList substitutedBody)
        _ -> Nothing

-- |
-- === Parameters
-- - `AST`: The entire AST (abstract syntax tree)
-- - `AST`: The function definition (including parameters and body)
-- - `AST`: The values to bind to the parameters
--
-- === Returns
-- - `Maybe AST`: The result of evaluating the function
-- - `Nothing`: if binding or evaluation fails
handleFunctions :: AST -> AST -> AST -> Maybe AST
handleFunctions ast (SList [SSymbol "define", SSymbol _, SList [SSymbol "lambda", params, body]]) values =
    case bindParameters params values of
        Just bindings -> returnValueHandleFunction ast bindings body
        Nothing -> Nothing
handleFunctions inast (SList [SSymbol "define", SList (SSymbol _ : params), body]) values =
        case bindParameters (SList params) values of
            Just bindings -> returnValueHandleFunction inast bindings body
            Nothing -> Nothing
handleFunctions ast (SList [params, SList body]) values =
    case bindParameters params values of
        Just bindings -> returnValueHandleFunction ast bindings (SList body)
        Nothing -> Nothing
handleFunctions _ _ _ = Nothing

-- | Return the given `Maybe AST` but in AST.
--
-- If AST exist, the result is printed. Otherwise, an error message is displayed.
--
-- ==== Parameters
-- - `Maybe AST`: The input `AST` to handle. If `Nothing`, an error message is displayed.
--
-- === Returns
-- - `AST`: AST with no Maybe
delMaybeAST :: Maybe AST -> AST
delMaybeAST (Just a) = a
delMaybeAST Nothing = SSymbol "ERROR no AST"

-- | Handles and evaluates the given `AST`.
--
-- If evaluation is successful, the result is printed. Otherwise, an error message is displayed.
--
-- ==== Parameters
-- - `Maybe AST`: The input `AST` to handle. If `Nothing`, an error message is displayed.
handleAST :: Maybe AST -> IO ()
handleAST Nothing = putStrLn "ERROR: Failed to parse check your Lisp expression!"
handleAST (Just a)
    | returnValueAST a a /= Nothing = print (delMaybeAST (returnValueAST a a))
    | otherwise = putStrLn "ERROR: Failed no return value!"
