module Main (main) where

import Lib (checkArgs, litostr, needParenthese)
import Parser.ParserLispSExp (parseSExpr)
import Parser.ParserSExpAST (parseAST)
import HandleAST.HandleAST (handleAST)

main :: IO ()
main = do
  input <- checkArgs
  handleAST (parseAST (parseSExpr (needParenthese (litostr input))))