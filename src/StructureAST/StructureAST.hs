module StructureAST.StructureAST (AST (..)) where

data AST = SInt Int | SSymbol String | SList [AST] | SBool Bool

instance Eq AST where
  SInt x == SInt y = x == y
  SSymbol x == SSymbol y = x == y
  SBool x == SBool y = x == y
  SList x == SList y = x == y
  _ == _ = False

instance Show AST where
  show (SInt i) = "int : " ++ show i
  show (SSymbol str) = "str : " ++ str
  show (SBool b) = if b then "bool : #t" else "bool : #f"
  show (SList xs) = "(" ++ unwords (map show xs) ++ ")"
