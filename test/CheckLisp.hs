{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- checkLisp
-}

import Data.Char (isDigit, digitToInt)

checkLisp :: String -> [String]
checkLisp str =
    let array = whiteSpaceMode str
    in case array of
        "" -> ["Warning: input is empty."]
        _ -> checkLisp' array "" 0

whiteSpaceMode :: String -> String
whiteSpaceMode = reverse
                . dropWhile (\c -> c == ' ' || c == '\n')
                . reverse
                . dropWhile (\c -> c == ' ' || c == '\n')

checkLisp' :: String -> String -> Int -> [String]
checkLisp' "" str count = case count of
    0 -> ["Text is valid LISP code."]
    _ -> ["Warning: input miss " ++ show count ++ " parenthesis."]
checkLisp' ('\n':xs) str count = checkLisp' xs "" count
checkLisp' ('(':xs) str count = checkLisp' xs (str ++ "(") (count + 1)
checkLisp' (')':xs) str count = checkLisp' xs (str ++ ")") (count - 1)
checkLisp' (x:"") str count
    | isDigit x =
        let digits = reverse . takeWhile isDigit . reverse $ str
            (rest, check) = checkNumber (digits ++ [x]) 0
        in case check of
            True -> checkLisp' "" rest count
            False ->
                let errorLine = getWholeLine str [x]
                in ["Warning: too long int at line:", errorLine]
    | otherwise = checkLisp' "" str count
checkLisp' (x:y:xs) str count
    | isDigit y = checkLisp' (y:xs) (str ++ [x]) count
    | isDigit x =
        let digits = reverse . takeWhile isDigit . reverse $ str
            (rest, check) = checkNumber (digits ++ [x]) 0
        in case check of
            True -> checkLisp' (y:xs) rest count
            False ->
                let errorLine = getWholeLine str (x:y:xs)
                in ["Warning: too long int at line:", errorLine]
    | otherwise = checkLisp' (y:xs) (str ++ [x]) count

checkNumber :: String -> Int -> (String, Bool)
checkNumber (x:xs) number
    | isDigit x =
        let n = digitToInt x
        in if (maxBound :: Int) - number * 10 - n < 0
           then (xs, False)
           else checkNumber xs (number * 10 + n)
    | otherwise = (x:xs, True)
checkNumber [] number = ([], True)

getWholeLine :: String -> String -> String
getWholeLine str xs =
    let before = reverse . takeWhile (/= '\n') . reverse $ str
        after = takeWhile (/= '\n') xs
    in before ++ after
