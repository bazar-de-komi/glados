data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show)

SList [SSymbol "define", SSymbol "x", SInt 5]
SSymbol "x"
SList [SSymbol "if",
    SList [SSymbol ">", SSymbol "x", SInt 4],
    SInt 1,
    SInt 0]
SList [SSymbol "define", SSymbol "y",
    SList [SSymbol "+", SInt 5, SSymbol "x"]]
