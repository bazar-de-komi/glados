{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar c = \input -> case input of
    (x:xs) -> if x == c then Just (c, xs) else Nothing
    [] -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar chars = \input -> case input of
    (x:xs) -> if x `elem` chars then Just (x, xs) else Nothing
    [] -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = \input -> case p1 input of
    Just result -> Just result
    Nothing -> p2 input

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = \input -> case p1 input of
    Just (result1, rest1) -> case p2 rest1 of
        Just (result2, rest2) -> Just ((result1, result2), rest2)
        Nothing -> Nothing
    Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = \input -> case p1 input of
    Just (result1, rest1) -> case p2 rest1 of
        Just (result2, rest2) -> Just (f result1 result2, rest2)
        Nothing -> Nothing
    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = \input -> case p input of
    Just (result, rest) -> case parseMany p rest of
        Just (results, finalRest) -> Just (result : results, finalRest)
    Nothing -> Just ([], input)

parseSome :: Parser a -> Parser [a]
parseSome p = parseAndWith (:) p (parseMany p)

parseUInt :: Parser Int
parseUInt = \input -> case parseSome (parseAnyChar ['0'..'9']) input of
    Just (digits, rest) -> Just (read digits, rest)
    Nothing -> Nothing

parseInt :: Parser Int
parseInt = \input -> case parseOr (parseChar '-') (parseChar '+') input of
    Just (sign, rest) -> case parseUInt rest of
        Just (num, finalRest) -> Just (if sign == '-' then -num else num, finalRest)
        Nothing -> Nothing
    Nothing -> parseUInt input

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = \input -> case parseAndWith (\_ (x, rest1) -> (x, rest1))
-- parseTuple :: Parser a -> Parser (a, a)
-- parseTuple p = \input -> case parseAndWith (\_ (x, rest1) -> (x, rest1))
--     (parseChar '(')
--     (parseAndWith (\x rest -> (x, rest))
--         p
--         (parseAndWith (\_ (y, rest2) -> (y, rest2))
--             (parseChar ',')
--             (parseAndWith (\y _ -> (y, ""))
--                 p
--                 (parseChar ')'))))
--     input

-- parseTuple :: Parser a -> Parser (a, a)
-- parseTuple p = \input -> case parseAndWith (\_ (x, rest1) ->
--                                            (x, rest1)). user ->
-- ``` ->
