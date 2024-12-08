{-# LANGUAGE LambdaCase #-}

module HandleAST.HandleFunctions (bindParameters, handleFunctions) where

import Structure (AST (..))

-- |
-- ==== Parameters
-- - `AST`: The parameters of the function
-- - `AST`: The value of the parameters
--
-- ==== Returns
-- `Maybe AST`: Return a list of the parameters bind
bindParameters :: AST -> AST -> Maybe AST
bindParameters (SList (parameter : paramTail)) (SList (value : valueTail)) =
    bindParameters (SList paramTail) (SList valueTail) >>= \case
        SList bindings
            -> Just (SList (SList [parameter, value] : bindings))
        _ -> Nothing
bindParameters (SList []) (SList []) = Just (SList [])
bindParameters _ _ = Nothing

-- |
-- ==== Parameters
-- - `AST`: Must be a copy of the entire ast
-- - `AST`: Must be the entire function
-- - `AST`: Must be the value of the parameters
--
-- ==== Returns
-- `Maybe AST`: Return the result of the function
handleFunctions :: AST -> AST -> AST -> Maybe AST
handleFunctions ast (SList [SSymbol "define", SSymbol _, SList [SSymbol "lambda", params, body]]) values =
    bindParameters params values
handleFunctions ast (SList [SSymbol "define", SList [SSymbol _, params], body]) values =
    bindParameters params values
handleFunctions ast (SList [SSymbol "lambda", params, body]) values =
    bindParameters params values
handleFunctions _ _ _ = Nothing
