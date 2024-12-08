module HandleAST.HandleFunctions (
    bindParameters,
    findBinding,
    substituteBindings,
    handleFunctions
) where

import Structure (AST (..))
import HandleAST.GetValue (getValue)
import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)

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
handleFunctions ast (SList [SSymbol "define", SList [SSymbol _, params], body]) values =
    case bindParameters params values of
        Just bindings -> returnValueHandleFunction ast bindings body
        Nothing -> Nothing
handleFunctions ast (SList [SSymbol "lambda", params, body]) values =
    case bindParameters params values of
        Just bindings -> returnValueHandleFunction ast bindings body
        Nothing -> Nothing
handleFunctions _ _ _ = Nothing
