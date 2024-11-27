module Main (main) where

import Lib(whilegetline, litostr)
import Parser_LISP_SE.Parserlispsexp(parseSExpr)
import Parser_SEXP_AST.Parsersexpast (parseAST)

main :: IO ()
main = do
  input <- whilegetline
  putStrLn $ "Résultat : "
  print (parseSExpr("(" ++ litostr input ++ ")"))