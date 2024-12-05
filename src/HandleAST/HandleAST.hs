module HandleAST.HandleAST (handleAST) where

import HandleAST.GetValue (getValue)
import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)
import Structure (AST(..))

returnValueAST::AST -> AST -> Maybe AST
returnValueAST _ (SInt a) = (Just (SInt a))
returnValueAST _ (SBool a) = (Just (SBool a))
returnValueAST inast (SSymbol a)
    | getValue inast (SSymbol a) == Nothing = (Just (SSymbol a))
    | otherwise = getValue inast (SSymbol a)
returnValueAST inast (SList (SList (SSymbol "define" : _) : a)) = returnValueAST inast (SList a)
returnValueAST inast (SList (SList a : _)) = returnValueAST inast (SList a)
returnValueAST inast (SList (SSymbol a : b : c : _))
    | a == "+" = add (returnValueAST inast b) (returnValueAST inast c)
    | a == "div" = divAST (returnValueAST inast b) (returnValueAST inast c)
    | a == "*" = multiply (returnValueAST inast b) (returnValueAST inast c)
    | a == "-" = subtractAST (returnValueAST inast b) (returnValueAST inast c)
    | a == "<" = lt (returnValueAST inast b) (returnValueAST inast c)
    | a == ">" = lt (returnValueAST inast c) (returnValueAST inast b)
    | a == "eq?" = eq (returnValueAST inast b) (returnValueAST inast c)
    | a == "mod" = modAST (returnValueAST inast b) (returnValueAST inast c)
    | otherwise = Nothing
returnValueAST _ _ = Nothing

handleAST::Maybe AST -> IO()
handleAST Nothing = putStrLn "ERROR: Failed to parse check your Lisp expression !"
handleAST (Just a)
    | returnValueAST a a /= Nothing = print (returnValueAST a a)
    | otherwise = putStrLn "ERROR: Failed no return value !"