module Main where

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

litostr::[String] -> String
litostr [] = ""
litostr(a:b) = a ++ litostr b

main :: IO ()
main = do
  input <- whilegetline
  putStrLn $ "Résultat : " ++ (litostr input)