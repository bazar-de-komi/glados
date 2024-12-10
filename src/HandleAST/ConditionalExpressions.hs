-- | Module for handling conditional expressions in an Abstract Syntax Tree (AST).
--
-- This module provides functions to:
-- 1. Evaluate conditional expressions using the `if` construct.
-- 2. Process conditions and evaluate branches based on the truth value of the condition.

module HandleAST.ConditionalExpressions (condExpress) where

import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)
import HandleAST.GetValue (getValue, getWithDefine)
-- import HandleAST.HandleAST
import Structure (AST(..))

-- | Evaluates a conditional expression (`if`).
--
-- This function processes an `if` expression in the form:
-- @
-- (if <CONDITION> <THEN_EXPR> <ELSE_EXPR>)
-- @
-- - It evaluates the condition (`cond`).
-- - If the condition evaluates to `True`, it evaluates and returns the `thenExpr`.
-- - Otherwise, it evaluates and returns the first element of `elseExpr`.
--
-- ==== Parameters
-- - `AST`: The environment in which to resolve variables or evaluate expressions.
-- - `AST`: The condition to evaluate (`cond`).
-- - `AST`: The expression to evaluate if the condition is `True` (`thenExpr`).
-- - `[AST]`: The list of expressions to evaluate if the condition is `False`. Only the first element is considered.
--
-- ==== Returns
-- - `Maybe AST`: The result of evaluating `thenExpr` or `elseExpr`, or `Nothing` if the evaluation fails.

condExpress :: AST -> AST -> AST -> [AST] -> Maybe AST
condExpress env cond thenExpr (elseExpr:_) =
    case evalCondition env cond of
        Just (SBool True)  -> evalExpression env thenExpr
        Just (SBool False) -> evalExpression env elseExpr
        _                  -> Nothing
condExpress _ _ _ [] = Nothing

-- | Evaluates a condition.
--
-- This function processes the condition part of an `if` expression.
-- Conditions can be:
-- - Booleans (e.g., `#t`, `#f`).
-- - Symbols resolved using the environment.
-- - Comparisons (e.g., `<`, `>`, `eq?`) applied to sub-expressions.
--
-- ==== Parameters
-- - `AST`: The environment for resolving symbols.
-- - `AST`: The condition expression to evaluate.
--
-- ==== Returns
-- - `Maybe AST`: A boolean result (`SBool True` or `SBool False`), or `Nothing` if the evaluation fails.

evalCondition :: AST -> AST -> Maybe AST
evalCondition _ (SBool b) = Just (SBool b)
evalCondition env (SSymbol sym) = getValue env (SSymbol sym)
evalCondition env (SList (SSymbol op : left : right : _))
    | op == "eq?" = eq (evalExpression env left) (evalExpression env right)
    | op == "<"   = lt (evalExpression env left) (evalExpression env right)
    | op == ">"   = lt (evalExpression env right) (evalExpression env left)
    | otherwise   = Nothing
evalCondition _ _ = Nothing

-- | Evaluates an expression.
--
-- This function handles the evaluation of various types of expressions, including:
-- - Integers, Booleans, and Symbols.
-- - Arithmetic operations (`+`, `-`, `*`, `div`, `mod`).
--
-- ==== Parameters
-- - `AST`: The environment for resolving symbols or variables.
-- - `AST`: The expression to evaluate.
--
-- ==== Returns
-- - `Maybe AST`: The evaluated result, or `Nothing` if evaluation fails.

evalExpression :: AST -> AST -> Maybe AST
evalExpression _ (SInt n) = Just (SInt n)
evalExpression _ (SBool b) = Just (SBool b)
evalExpression env (SSymbol sym) = getValue env (SSymbol sym)
evalExpression env (SList (SSymbol op : left : right : _))
    | op == "+"   = add (evalExpression env left) (evalExpression env right)
    | op == "-"   = subtractAST (evalExpression env left) (evalExpression env right)
    | op == "*"   = multiply (evalExpression env left) (evalExpression env right)
    | op == "div" = divAST (evalExpression env left) (evalExpression env right)
    | op == "mod" = modAST (evalExpression env left) (evalExpression env right)
    | otherwise   = Nothing
evalExpression _ _ = Nothing
