module Parser.ParserSExpAST (parseAST, isInt, isBool, finBool, noMaybeParseAST) where

import Structure (SExpr(..), AST(..))
import Data.Maybe (mapMaybe)

isInt::String -> Bool
isInt [] = True
isInt (a:b)
    | a == '0' = isInt b
    | a == '1' = isInt b
    | a == '2' = isInt b
    | a == '3' = isInt b
    | a == '4' = isInt b
    | a == '5' = isInt b
    | a == '6' = isInt b
    | a == '7' = isInt b
    | a == '8' = isInt b
    | a == '9' = isInt b
    | otherwise = False

isBool::String -> Bool
isBool (a:b:_) = a == '#' && (b == 'f' || b == 't')
isBool _ = False

finBool::String -> Bool
finBool (_:b:_) = b == 't'
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
