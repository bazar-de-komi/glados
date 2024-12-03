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
    case whiteSpaceMode str of
        "" -> trace "parseSExpr: string empty after removal of trailing spaces"
              Nothing
        ('(' : xs) ->
            case parseExpr ('(' : xs) of
            Just (expr, "") -> Just expr
            Just (_, rest) -> trace "parseSExpr: leftover input after parsing."
                              Nothing
            Nothing -> Nothing
        xs -> parseWrappedList str

parseWrappedList :: String -> Maybe SExpr
parseWrappedList str = parseSExpr ("(" ++ str ++ ")")

-- Parse an SExpr (either Atom or List)
parseExpr :: String -> Maybe (SExpr, String)
parseExpr "" = Nothing
parseExpr ('(' : xs) = do
    (list, rest) <- parseList xs
    return (List list, rest)
parseExpr xs =
    let str = dropWhile (== ' ') xs
    in if null str
       then trace "parseExpr: expr is null." Nothing
       else parseAtom str

parseList :: String -> Maybe ([SExpr], String)
parseList (')' : xs) = Just ([], dropWhile (== ' ') xs)
parseList "" = trace "Parsing error: unexpected end of input." Nothing
parseList xs = do
    let str = dropWhile (== ' ') xs
    (expr, rest) <- parseExpr str
    (list, finalRest) <- parseList rest
    return (expr : list, finalRest)

parseAtom :: String -> Maybe (SExpr, String)
parseAtom xs =
    let (atom, rest) = span (`notElem` "( )") xs
        trimmedRest  = dropWhile (== ' ') rest
    in if null atom
       then trace "empty atom encountered." Nothing
       else Just (Atom atom, trimmedRest)

whiteSpaceMode :: String -> String
whiteSpaceMode = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
