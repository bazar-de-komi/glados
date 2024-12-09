module HandleAST.FunctionsUtils (
    bindParameters,
    findBinding,
    substituteBindings
) where

import Structure (AST (..))

-- | Bind every functions parameter to the values set by the user
--
-- ==== Parameters
-- - `AST`: A list of parameters (i.e., symbols or variables) to be bound
-- - `AST`: A list of values that correspond to the parameters
--
-- ==== Returns
-- - `Maybe AST`: Return a `SList` of pairs `[parameter, value]` if successful
-- - `Nothing`: if the binder failed
bindParameters :: AST -> AST -> Maybe AST
bindParameters (SList (parameter : paramTail)) (SList (value : valueTail)) =
    case bindParameters (SList paramTail) (SList valueTail) of
        Just (SList binds) -> Just (SList (SList [parameter, value] : binds))
        _ -> Nothing
bindParameters (SList []) (SList []) = Just (SList [])
bindParameters _ _ = Nothing

-- | Return the value of a function parameter bind
--
-- ==== Parameters
-- - `String`: The target to search the value of the parameter in the bindings
-- - `AST`: The binding list
--
-- ==== Returns
-- - `Maybe AST`: The value of a function parameter bind
-- - `Nothing`: If a bind is not found
findBinding :: String -> AST -> Maybe AST
findBinding target (SList (SList [SSymbol param, value] : bindTail)) 
    | param == target = Just value
    | otherwise = findBinding target (SList bindTail)
findBinding _ _ = Nothing

-- | Take the body of the function and change every param call to its value
--
-- ==== Parameters
-- - `AST`: The list of bindings that associates parameters to values.
-- - `AST`: The `AST` to substitute bindings in.
--
-- ==== Returns
-- - `AST`: The `AST` with the substituted bindings.
-- - If a symbol is not found in the bindings, it returns the symbol itself.
substituteBindings :: AST -> AST -> AST
substituteBindings bindings (SSymbol param) =
    case findBinding param bindings of
        Just value -> value
        Nothing -> SSymbol param
substituteBindings binds (SList xs) = SList (map (substituteBindings binds) xs)
substituteBindings _ ast = ast
