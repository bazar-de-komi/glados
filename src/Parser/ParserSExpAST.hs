module Parser.ParserSExpAST (findBool, noMaybeParseAST) where

import Structure (SExpr(..), AST(..))
import Data.Maybe (mapMaybe)

findBool :: String -> Bool
findBool a = a == "True"

isCall :: [SExpr] -> Bool
isCall _ = False

findCall :: [SExpr] -> [AST]
findCall _ = []

noMaybeParseAST :: SExpr -> Maybe AST
noMaybeParseAST (List a)
    | isCall a = Just (SList (findCall a))
    | otherwise = Just . SList $ mapMaybe noMaybeParseAST a
noMaybeParseAST (Boolean a) = Just (SBool (findBool a))
noMaybeParseAST (SEFloat a) = Just (SFloat a)
noMaybeParseAST (SEInt a) = Just (SInt a)
noMaybeParseAST (SEChar a) = Just (SChar a)
noMaybeParseAST (Type a) = Just (SType a)
noMaybeParseAST (SEString a) = Just (SString a)
noMaybeParseAST (Atom a) = Just (SVariable a)
noMaybeParseAST (BasicFunc a) = Just (SOperation a)
noMaybeParseAST _ = Nothing