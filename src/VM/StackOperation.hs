module VM.StackOperation (
    Stack,
    emptyStack,
    pushValue,
    popValue,
    peekValue
) where

type Stack a = [a]

emptyStack :: Stack a
emptyStack = []

pushValue :: a -> Stack a -> Stack a
pushValue value stack = value : stack

popValue :: Stack a -> Either String (a, Stack a)
popValue [] = Left "Erreur : La pile est vide, impossible de retirer un élément."
popValue (x:xs) = Right (x, xs)

peekValue :: Stack a -> Either String a
peekValue [] = Left "Erreur : La pile est vide, impossible de consulter le sommet."
peekValue (x:_) = Right x
