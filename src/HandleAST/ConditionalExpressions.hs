module HandleAST.ConditionalExpressions (condExpress) where

import Structure (AST (..))

condExpress :: AST -> AST -> AST -> AST
condExpress (SBool True) thenExpr _ = thenExpr
condExpress (SBool False) _ elseExpr = elseExpr

