module Parser_SEXP_AST.Parsersexpast (parseAST) where

import StructureSE.StructureSE (SExpr(..))
import StructureAST.StructureAST (AST(..))
import Data.Maybe (mapMaybe)

isInt::String -> Bool
isInt [] = 1 == 1
isInt (a:b) = (a == '0' || a == '1' || a == '2' || a == '3' || a == '4' || a == '5' || a == '6' || a == '7' || a == '8' || a == '9') && isInt b

isBool::String -> Bool
isBool (a:b:_) = (a == '#' && (b == 'f' || b == 't'))
isBool _ = 1 == 2

finBool::String -> Bool
finBool (_:b:_) = b == 't'

noMaybeParseAST::SExpr -> Maybe AST
noMaybeParseAST (List a) = Just . SList $ mapMaybe noMaybeParseAST a
noMaybeParseAST (Atom a)
    | isInt a = Just (SInt (read a))
    | isBool a = Just (SBool (finBool a))
    | otherwise = Just (SSymbol a)

parseAST::Maybe SExpr -> Maybe AST
parseAST Nothing = Nothing
parseAST (Just (List a)) = Just . SList $ mapMaybe noMaybeParseAST (a)
parseAST (Just (Atom a))
    | isInt a = Just (SInt (read a))
    | isBool a = Just (SBool (finBool a))
    | otherwise = Just (SSymbol a)
