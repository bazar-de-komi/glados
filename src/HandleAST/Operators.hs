module HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST) where

import Structure (AST(..))

-- | Equality operator
eq :: AST -> AST -> Maybe AST
eq (SInt x) (SInt y) = Just $ SBool (x == y)
eq (SSymbol x) (SSymbol y) = Just $ SBool (x == y)
eq (SBool x) (SBool y) = Just $ SBool (x == y)
eq _ _ = Nothing

-- | Less-than operator
lt :: AST -> AST -> Maybe AST
lt (SInt x) (SInt y) = Just $ SBool (x < y)
lt (SSymbol x) (SSymbol y) = Just $ SBool (x < y)
lt _ _ = Nothing

-- | Addition operator
add :: Maybe AST -> Maybe AST -> Maybe AST
add (Just(SInt x)) (Just (SInt y)) = Just $ SInt (x + y)
add (Just(SSymbol x)) (Just(SSymbol y)) = Just $ SSymbol (x ++ y)
add _ _ = Nothing

-- | Subtraction operator
subtractAST :: AST -> AST -> Maybe AST
subtractAST (SInt x) (SInt y) = Just $ SInt (x - y)
subtractAST _ _ = Nothing

-- | Multiplication operator
multiply :: AST -> AST -> Maybe AST
multiply (SInt x) (SInt y) = Just $ SInt (x * y)
multiply _ _ = Nothing

-- | Division operator
divAST :: AST -> AST -> Maybe AST
divAST (SInt x) (SInt y)
    | y == 0 = Nothing
    | otherwise = Just $ SInt (x `div` y)
divAST _ _ = Nothing

-- | Modulo operator
modAST :: AST -> AST -> Maybe AST
modAST (SInt x) (SInt y)
    | y == 0 = Nothing
    | otherwise = Just $ SInt (x `mod` y)
modAST _ _ = Nothing