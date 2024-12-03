module Parser_SEXP_AST.Parsersexpast (parseAST, isInt, isBool, finBool, noMaybeParseAST) where

import StructureSE.StructureSE (SExpr(..))
import StructureAST.StructureAST (AST(..))
import Data.Maybe (mapMaybe)

isInt::String -> Bool
isInt [] = True
isInt (a:b) = (a == '0' || a == '1' || a == '2' || a == '3' || a == '4' || a == '5' || a == '6' || a == '7' || a == '8' || a == '9') && isInt b

isBool::String -> Bool
isBool (a:b:_) = (a == '#' && (b == 'f' || b == 't'))
isBool _ = False

finBool::String -> Bool
finBool (_:b:_) = (b == 't')
finBool _ = False

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
