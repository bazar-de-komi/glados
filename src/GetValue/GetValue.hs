module GetValue.GetValue (getValue) where

import StructureAST.StructureAST (AST (..))

getValue :: AST -> AST -> Maybe AST
getValue (SInt _) _ = Nothing
getValue (SBool _) _ = Nothing
getValue (SSymbol _) _ = Nothing
getValue (SList []) _ = Nothing
getValue (SList ((SList (SSymbol "define" : aFilter : value : _)) : xs)) target
  | aFilter == target = Just value
  | otherwise = getValue (SList xs) target
getValue (SList (_: itemTail)) target = getValue (SList itemTail) target
