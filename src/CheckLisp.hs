module CheckLisp (checkLisp) where

import Data.Char (isDigit, digitToInt)

checkLisp :: String -> String
checkLisp str =
    let array = whiteSpaceMode str
    in case array of
        "" -> "Warning: input is empty."
        _ -> checkLisp' array "" 0 0

whiteSpaceMode :: String -> String
whiteSpaceMode = reverse
                . dropWhile (\c -> c == ' ' || c == '\n')
                . reverse
                . dropWhile (\c -> c == ' ' || c == '\n')

checkLisp' :: String -> String -> Int -> Int -> String
checkLisp' "" _ 0 0 = "OK"
checkLisp' "" _ _ 1 = "Warning: input miss 1 quote."
checkLisp' "" _ p _ = "Warning: input miss " ++ show p ++ " parenthesis."

checkLisp' ('\n':xs) _ p q = checkLisp' xs "" p q
checkLisp' ('\"':xs) str p 0 = checkLisp' xs str p 1
checkLisp' ('\"':xs) str p 1 = checkLisp' xs str p 0
checkLisp' (_:xs) str p 1 = checkLisp' xs str p 1
checkLisp' ('(':xs) str p q = checkLisp' xs (str ++ "(") (p + 1) q
checkLisp' (')':xs) str p q = checkLisp' xs (str ++ ")") (p - 1) q

checkLisp' (x:"") str p q
    | isDigit x =
        let digits = reverse . takeWhile isDigit . reverse $ str
            (rest, check) = checkNumber (digits ++ [x]) 0
        in case check of
            True -> checkLisp' "" rest p q
            False ->
                let errorLine = getWholeLine str [x]
                in "Warning: too long int at line:\n" ++ errorLine
    | otherwise = checkLisp' "" str p q
checkLisp' (x:y:xs) str p q
    | isDigit y = checkLisp' (y:xs) (str ++ [x]) p q
    | isDigit x =
        let digits = reverse . takeWhile isDigit . reverse $ str
            (rest, check) = checkNumber (digits ++ [x]) 0
        in case check of
            True -> checkLisp' (y:xs) rest p q
            False ->
                let errorLine = getWholeLine str (x:y:xs)
                in "Warning: too long int at line:\n" ++ errorLine
    | otherwise = checkLisp' (y:xs) (str ++ [x]) p q

checkNumber :: String -> Int -> (String, Bool)
checkNumber (x:xs) number
    | isDigit x =
        let n = digitToInt x
        in if (maxBound :: Int) - number * 10 - n < 0
           then (xs, False)
           else checkNumber xs (number * 10 + n)
    | otherwise = (x:xs, True)
checkNumber [] _ = ([], True)

getWholeLine :: String -> String -> String
getWholeLine str xs =
    let before = reverse . takeWhile (/= '\n') . reverse $ str
        after = takeWhile (/= '\n') xs
    in before ++ after
