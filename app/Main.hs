module Main (main) where

import Lib (checkArgs, litostr, needParenthese)
import Parser_LISP_SE.Parserlispsexp (parseSExpr)
import Parser_SEXP_AST.Parsersexpast (parseAST)

main :: IO ()
main = do
  input <- checkArgs
  putStrLn $ "Résultat : "
  print (parseAST (parseSExpr (needParenthese (litostr input))))