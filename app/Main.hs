module Main (main) where

import System.IO
import Parser_LISP_SE.Parserlispsexp(parseSExpr)
import StructureSE.StructureSE (SExpr(..))

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
  putStrLn $ "Résultat : "
  print (parseSExpr(litostr input))