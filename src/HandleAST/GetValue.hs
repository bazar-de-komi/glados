-- | Module for extracting values from an Abstract Syntax Tree (AST).
-- This module provides functions to search and retrieve values within the AST.

module HandleAST.GetValue (getValue, getWithDefine) where

import Structure (AST (..))

-- | Search for a target value within sublists of an `AST`.
--
-- The first parameter is the list of `AST` nodes.
-- The second parameter is the target `AST` to search for.
--
-- ==== Returns
-- A `Maybe AST` containing the value if the target is found, or `Nothing` otherwise.
searchInSubLists :: AST -> AST -> Maybe AST
searchInSubLists list target = Just =<< getValue list target

-- | Retrieve the value associated with a `define` operator in the AST.
--
-- The first parameter is the entire `AST`.
-- The second parameter is the target `AST` to search for.
--
-- ==== Behavior
-- - If the target is defined using the `define` operator, its value is returned.
-- - If the target is not found, the function recursively searches through sublists.
--
-- ==== Returns
-- A `Maybe AST` containing the value if found, or `Nothing` otherwise.
getValue :: AST -> AST -> Maybe AST
getValue (SList (SSymbol "define" : aFilter : value : _)) target
    | aFilter == target = Just value
getValue (SList (item : itemTail)) target =
    case searchInSubLists item target of
        Just result -> Just result
        Nothing -> getValue (SList itemTail) target
getValue _ _ = Nothing

getWithDefine :: AST -> AST -> Maybe AST
getWithDefine (SList (x:xs)) target =
    case x of
        SList (SSymbol "define" : SSymbol a : _) 
            | SSymbol a == target -> Just x
        SList (SSymbol "define" : SList (SSymbol a : _) : _) 
            | SSymbol a == target -> Just x
        _ -> getWithDefine (SList xs) target
getWithDefine _ _ = Nothing
