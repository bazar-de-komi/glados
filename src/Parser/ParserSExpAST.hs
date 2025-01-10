module Parser.ParserSExpAST (findBool, noMaybeParseAST) where

import Structure (SExpr(..), AST(..))
import Data.Maybe (mapMaybe)

findBool :: String -> Bool
findBool a = a == "True"

findFuncOrDef :: [SExpr] -> Maybe AST
findFuncOrDef (Atom n : Type a : Param b : List r : []) =
    Just (SFunc n (SType a) (SList $ mapMaybe noMaybeParseAST b) (SList $ mapMaybe noMaybeParseAST r))
findFuncOrDef (Atom n : Type a : Param b : r) =
    Just (SFunc n (SType a) (SList $ mapMaybe noMaybeParseAST b) (SList $ mapMaybe noMaybeParseAST r))
findFuncOrDef (Atom n : Type a : List r : []) =
    Just (SDefine n (SType a) (SList $ mapMaybe noMaybeParseAST r))
findFuncOrDef (Atom n : Type a : r) =
    Just (SDefine n (SType a) (SList $ mapMaybe noMaybeParseAST r))
findFuncOrDef _ = Nothing

noMaybeParseAST :: SExpr -> Maybe AST
noMaybeParseAST (Boolean a) = Just (SBool (findBool a))
noMaybeParseAST (SEFloat a) = Just (SFloat a)
noMaybeParseAST (SEInt a) = Just (SInt a)
noMaybeParseAST (SEChar a) = Just (SChar a)
noMaybeParseAST (Type a) = Just (SType a)
noMaybeParseAST (SEString a) = Just (SString a)
noMaybeParseAST (Atom a) = Just (SVariable a)
noMaybeParseAST (BasicFunc a) = Just (SOperation a)
noMaybeParseAST (List (Atom n : BasicFunc ":" : rest)) = (findFuncOrDef (Atom n : rest))
noMaybeParseAST (List a) = Just . SList $ mapMaybe noMaybeParseAST a
noMaybeParseAST _ = Nothing