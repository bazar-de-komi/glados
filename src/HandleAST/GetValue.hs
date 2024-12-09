-- | Module for extracting values from an Abstract Syntax Tree (AST).
-- This module provides functions to search and retrieve values within the AST.

module HandleAST.GetValue (getValue, getWithDefine) where

import Structure (AST (..))

-- | Search for a target value within sublists of an `AST`
--
-- ==== Parameters
-- - `AST`: The first parameter is the list of `AST` nodes
-- - `AST`: The second parameter is the target `AST` to search for
--
-- ==== Returns
-- - `Maybe AST`: containing the value if the target is found
-- - `Nothing`: if there's no more sub list
searchInSubLists :: AST -> AST -> Maybe AST
searchInSubLists list target = Just =<< getValue list target

-- | Retrieve the value associated with a `define` operator in the AST
--
-- The first parameter is the entire `AST`
-- The second parameter is the target `AST` to search for
--
-- ==== Behavior
-- - If the target is defined using the `define` operator, its value is returned
-- - If the target is not found, the function recursively searches through sublists
--
-- ==== Returns
-- - `Maybe AST`: containing the value if found
-- - `Nothing`: when the variable doesn't exist
getValue :: AST -> AST -> Maybe AST
getValue (SList (SSymbol "define" : aFilter : value : _)) target
    | aFilter == target = Just value
getValue (SList (item : itemTail)) target =
    case searchInSubLists item target of
        Just result -> Just result
        Nothing -> getValue (SList itemTail) target
getValue _ _ = Nothing

-- | Retrieve the entire `define` operator in the AST for the functions
--
-- The first parameter is the entire `AST`
-- The second parameter is the target `AST` to search for
--
-- ==== Behavior
-- - If the target is defined using the `define` operator, the entire `define` operator is returned
-- - If the target is not found, the function recursively searches through sublists
--
-- ==== Returns
-- - `Maybe AST`: Return the variable if found
-- - `Nothing`: when the function doesn't exist
getWithDefine :: AST -> AST -> Maybe AST
getWithDefine (SList (x:xs)) target =
    case x of
        SList (SSymbol "define" : SSymbol a : _) 
            | SSymbol a == target -> Just x
        SList (SSymbol "define" : SList (SSymbol a : _) : _) 
            | SSymbol a == target -> Just x
        _ -> getWithDefine (SList xs) target
getWithDefine _ _ = Nothing
