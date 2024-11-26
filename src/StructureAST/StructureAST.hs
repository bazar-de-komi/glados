module StructureAST.StructureAST (AST(..)) where

data AST = SInt Int | SSymbol String | SList [AST] | SBool Bool deriving (Show)