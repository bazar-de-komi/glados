{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ParsetoSExp
-}

import Debug.Trace (trace)

data SExpr = Atom String
           | List [SExpr]
           deriving (Show)

-- Main function to parse the string into Maybe SExpr
parseSExpr :: String -> Maybe SExpr
parseSExpr str = 
    case parseExpr (whiteSpaceMode str) of
        Just (expr, "") -> Just expr
        Just (_, rest) -> trace "parseSExpr: leftover input after parsing." Nothing
        Nothing -> trace "parseSExpr: failed to parse as a single expression, attempting to wrap input as a list." 
                   parseWrappedList str

parseWrappedList :: String -> Maybe SExpr
parseWrappedList str = parseSExpr ("(" ++ str ++ ")")

-- Parse an SExpr (either Atom or List)
parseExpr :: String -> Maybe (SExpr, String)
parseExpr "" = Nothing
parseExpr ('(' : xs) = do
    (list, rest) <- parseList xs
    return (List list, rest)
parseExpr xs =  let str = dropWhile (== ' ') xs
                in if null str
                   then trace "parseExpr: expr is null." Nothing
                   else parseAtom str

parseList :: String -> Maybe ([SExpr], String)
parseList (')' : xs) = Just ([], xs)
parseList "" = trace "Parsing error: unexpected end of input." Nothing
parseList xs = do
    (expr, rest) <- parseExpr xs
    (list, finalRest) <- parseList rest
    return (expr : list, finalRest)

parseAtom :: String -> Maybe (SExpr, String)
parseAtom xs = case span (`notElem` "( )") xs of
    ("", _) -> trace "empty atom encountered." Nothing
    (atom, rest) -> Just (Atom atom, rest)

whiteSpaceMode :: String -> String
whiteSpaceMode = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
