module Stack (
    Stack,
    emptyStack,
    push,
    pop,
    peek
) where

type Stack a = [a]

emptyStack :: Stack a
emptyStack = []

push :: a -> Stack a -> Stack a
push value stack = value : stack

pop :: Stack a -> Either String (a, Stack a)
pop [] = Left "Erreur : La pile est vide, impossible de retirer un élément."
pop (x:xs) = Right (x, xs)

peek :: Stack a -> Either String a
peek [] = Left "Erreur : La pile est vide, impossible de consulter le sommet."
peek (x:_) = Right x
