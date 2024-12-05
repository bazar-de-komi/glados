module HandleAST.GetValue (getValue) where

import Structure (AST (..))

-- | Search the target value in every sub lists of a list
--
-- The first parameter must be a list of AST,
-- the second parameter must be a target AST,
-- the function return the value if the target was found,
-- otherwise it returns Nothing
searchInSubLists :: AST -> AST -> Maybe AST
searchInSubLists list target = Just =<< getValue list target

-- | Get the target value of a define operator
--
-- The first parameter must be the entire generated AST,
-- the second parameter must be a target AST,
-- the function return the value if the target was found,
-- otherwise it returns Nothing
getValue :: AST -> AST -> Maybe AST
getValue (SList (SSymbol "define" : aFilter : value : _)) target
    | aFilter == target = Just value
getValue (SList (item : itemTail)) target =
    case searchInSubLists item target of
        Just result -> Just result
        Nothing -> getValue (SList itemTail) target
getValue _ _ = Nothing
