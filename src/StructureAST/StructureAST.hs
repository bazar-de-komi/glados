module StructureAST.StructureAST (AST(..)) where

data AST = SInt Int | SSymbol String | SList [AST] | SBool Bool deriving (Eq)

instance Show AST where
    show (SInt i) = show i
    show (SSymbol str) = str
    show (SBool b) = if b then "#t" else "#f"
    show (SList xs)  = "(" ++ unwords (map show xs) ++ ")"