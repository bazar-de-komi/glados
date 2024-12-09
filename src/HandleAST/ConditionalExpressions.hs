module HandleAST.ConditionalExpressions (condExpress) where

import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)
import HandleAST.GetValue (getValue)
import Structure (AST(..))

condExpress :: AST -> AST -> AST -> [AST] -> Maybe AST
condExpress env cond thenExpr (elseExpr:_) =
    case evalCondition env cond of
        Just (SBool True)  -> evalExpression env thenExpr
        Just (SBool False) -> evalExpression env elseExpr
        _                  -> Nothing

evalCondition :: AST -> AST -> Maybe AST
evalCondition env (SBool b) = Just (SBool b)
evalCondition env (SSymbol sym) = getValue env (SSymbol sym)
evalCondition env (SList (SSymbol op : left : right : _))
    | op == "eq?" = eq (evalExpression env left) (evalExpression env right)
    | op == "<"   = lt (evalExpression env left) (evalExpression env right)
    | op == ">"   = lt (evalExpression env right) (evalExpression env left)
    | otherwise   = Nothing
evalCondition _ _ = Nothing

evalExpression :: AST -> AST -> Maybe AST
evalExpression env (SInt n) = Just (SInt n)
evalExpression env (SBool b) = Just (SBool b)
evalExpression env (SSymbol sym) = getValue env (SSymbol sym)
evalExpression env (SList (SSymbol op : left : right : _))
    | op == "+"   = add (evalExpression env left) (evalExpression env right)
    | op == "-"   = subtractAST (evalExpression env left) (evalExpression env right)
    | op == "*"   = multiply (evalExpression env left) (evalExpression env right)
    | op == "div" = divAST (evalExpression env left) (evalExpression env right)
    | op == "mod" = modAST (evalExpression env left) (evalExpression env right)
    | otherwise   = Nothing
evalExpression _ _ = Nothing