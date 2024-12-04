data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show)

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol string) = Just string
getSymbol _ = Nothing

getInt :: SExpr -> Maybe Int
getInt (SInt int) = Just int
getInt _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList sexpr) = Just sexpr
getList _ = Nothing

main :: IO ()
main = do
    let sym = SSymbol "x"
    let int = SInt 5
    let list = SList [SSymbol "y", SInt 10]
    
    print (getSymbol sym)
    print (getSymbol int)
    print (getSymbol list)
