module HandleAST.HandleAST (handleAST) where

import HandleAST.GetValue (getValue)
import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)
import Structure (AST(..))

returnValueAST::AST -> Maybe AST
returnValueAST (SInt a) = (Just (SInt a))
returnValueAST (SBool a) = (Just (SBool a))
returnValueAST (SSymbol a) = (Just (SSymbol a))
returnValueAST (SList (SSymbol a : b : c : _))
    | a == "+" = add (returnValueAST b) (returnValueAST c)
    | otherwise = Nothing
returnValueAST _ = Nothing

handleAST::Maybe AST -> IO()
handleAST Nothing = putStrLn "ERROR: Failed to parse check your Lisp expression !"
handleAST (Just a)
    | returnValueAST a /= Nothing = print (returnValueAST a)
    | otherwise = putStrLn "ERROR: Failed no return value !"