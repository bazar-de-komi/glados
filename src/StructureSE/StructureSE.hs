module StructureSE.StructureSE (SExpr(..)) where

data SExpr = Atom String | List [SExpr] deriving (Eq)

instance Show SExpr where
    show (Atom str) = str
    show (List xs)  = "(" ++ unwords (map show xs) ++ ")"