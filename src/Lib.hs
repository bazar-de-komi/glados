module Lib (checkArgs, litostr) where
import System.IO
import System.Environment

checkFlag::[String] -> Bool
checkFlag args = "-i" `elem` args

tailOf::[String] -> String
tailOf [] = []
tailOf (a:b)
  | b == [] = a
  | otherwise = tailOf b

checkArgs::IO [String]
checkArgs = do
  args <- getArgs
  if checkFlag args
    then do
      let fileName = tailOf args
      content <- readFile fileName
      return (lines content)
    else do
      ret <- whilegetline
      return ret

whilegetline::IO [String]
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