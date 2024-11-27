module Lib (whilegetline, litostr) where
import System.IO

whilegetline :: IO [String]
whilegetline = do
  isClosed <- isEOF
  if isClosed
    then return []
    else do
      player1 <- getLine
      rest <- whilegetline
      return (player1 : rest)

checkparenthese::String -> String
checkparenthese [] = []
checkparenthese (a:b)
    | a == '(' = ' ' : a : checkparenthese b
    | otherwise = a : checkparenthese b

litostr::[String] -> String
litostr [] = ""
litostr(a:b) = (checkparenthese a) ++ " " ++ litostr b