module HandleAST.HandleFunctions (
    bindParameters,
    findBinding,
    substituteBindings,
    handleFunctions,
    returnValueAST
) where

import Structure (AST (..))
import HandleAST.GetValue (getValue, getWithDefine)
import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)

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
returnValueAST inast (SList (SList (SSymbol "define" : _) : a)) = returnValueAST inast (SList a)
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

-- |
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

-- |
-- ==== Parameters
-- - `String`: The target to search the value of the parameter in the bindings
-- - `AST`: The binding list
--
-- ==== Returns
-- - `AST`: The list of the bind we want
findBinding :: String -> AST -> Maybe AST
findBinding target (SList (SList [SSymbol param, value] : bindTail)) 
    | param == target = Just value
    | otherwise = findBinding target (SList bindTail)
findBinding _ _ = Nothing

-- |
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
