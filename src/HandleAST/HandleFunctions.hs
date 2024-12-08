{-# LANGUAGE LambdaCase #-}

module HandleAST.HandleFunctions (bindParameters) where

import Structure (AST (..))

-- | 
-- ==== Parameters
-- `AST`: The parameters of the function
-- `AST`: The value of the parameters
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
