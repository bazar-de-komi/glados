data SExpr = SInt Int
           | SSymbol String
           | SList [SExpr]
           deriving (Show)

printTree :: SExpr -> Maybe String
printTree (SSymbol string) = Just $ "a Symbol '" ++ show string ++ "'"
printTree (SInt int) = Just $ "a Number " ++ show int
printTree (SList []) = Just "an empty List"
printTree (SList (elem:list)) =
    case printTree elem of
        Nothing -> Nothing
        Just head ->
            let rest = map printTree list
                rest' = sequence rest
            in case rest' of
                Nothing -> Nothing
                Just restStrs ->
                    Just $ "a List with " ++ head ++
                        (if not (null list)
                            then " followed by " ++ unwords (map (", " ++) restStrs)
                            else "")

-- main :: IO ()
-- main = do
--     let list = SList [SSymbol "define", SSymbol "x", SInt 5]
    
--     print printTree list
