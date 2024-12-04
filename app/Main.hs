module Main (main) where

import Lib (checkArgs, litostr, needParenthese)
import Parser.ParserLispSExp (parseSExpr)
import Parser.ParserSExpAST (parseAST)

main :: IO ()
main = do
  input <- checkArgs
  putStrLn $ "Résultat : "
  print (parseAST (parseSExpr (needParenthese (litostr input))))