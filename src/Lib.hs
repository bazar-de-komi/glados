module Lib (checkArgs, litostr, needParenthese) where
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

checkNotEnd::String -> Bool
checkNotEnd [] = False
checkNotEnd _ = True

checkAllString::String -> Int -> Bool
checkAllString [] _ = False
checkAllString (a:b) i
    | a == ')' && i == 0 = checkNotEnd b
    | a == ')' = checkAllString b (i - 1)
    | a == '(' = checkAllString b (i + 1)
    | otherwise = checkAllString b i

needParenthese::String -> String
needParenthese [] = []
needParenthese a
  | checkAllString a 0 = "(" ++ a ++ ")"
  | otherwise = a