module HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST) where

import Structure (AST(..))

-- | Equality operator
eq :: Maybe AST -> Maybe AST -> Maybe AST
eq (Just (SInt x)) (Just (SInt y)) = Just $ SBool (x == y)
eq (Just (SSymbol x)) (Just (SSymbol y)) = Just $ SBool (x == y)
eq (Just (SBool x)) (Just (SBool y)) = Just $ SBool (x == y)
eq _ _ = Nothing

-- | Less-than operator
lt :: Maybe AST -> Maybe AST -> Maybe AST
lt (Just (SInt x)) (Just (SInt y)) = Just $ SBool (x < y)
lt (Just (SSymbol x)) (Just (SSymbol y)) = Just $ SBool (x < y)
lt _ _ = Nothing

-- | Addition operator
add :: Maybe AST -> Maybe AST -> Maybe AST
add (Just (SInt x)) (Just (SInt y)) = Just $ SInt (x + y)
add (Just (SSymbol x)) (Just (SSymbol y)) = Just $ SSymbol (x ++ y)
add _ _ = Nothing

-- | Subtraction operator
subtractAST :: Maybe AST -> Maybe AST -> Maybe AST
subtractAST (Just (SInt x)) (Just (SInt y)) = Just $ SInt (x - y)
subtractAST _ _ = Nothing

-- | Multiplication operator
multiply :: Maybe AST -> Maybe AST -> Maybe AST
multiply (Just (SInt x)) (Just (SInt y)) = Just $ SInt (x * y)
multiply _ _ = Nothing

-- | Division operator
divAST :: Maybe AST -> Maybe AST -> Maybe AST
divAST (Just (SInt x)) (Just (SInt y))
    | y == 0 = Nothing
    | otherwise = Just $ SInt (x `div` y)
divAST _ _ = Nothing

-- | Modulo operator
modAST :: Maybe AST -> Maybe AST -> Maybe AST
modAST (Just (SInt x)) (Just (SInt y))
    | y == 0 = Nothing
    | otherwise = Just $ SInt (x `mod` y)
modAST _ _ = Nothing