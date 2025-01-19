module ParserSExpASTSpec (spec) where

import Test.Hspec
import Parser.ParserSExpAST (parseFinalAST, lastCheck)
import Parser.ParserKleftisSExp (pProgram)
import Structure (AST(..))
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "Functional Tests for ParserSExpAST" $ do

    it "Parses and processes a program with basic definitions and functions" $ do
      let input = "coucou : ent (9)\n\ncarre : ent [ent (a)] (result a * a)"
      case parse pProgram "" input of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ast = parseFinalAST sexpr
          case lastCheck ast of
            Left err -> expectationFailure $ "AST validation failed: " ++ err
            Right finalAST -> finalAST `shouldBe` SList [SDefine "coucou" (SType "int") (SList [SInt 9]), SFunc "carre" (SType "int") (SList [SType "int", SList [SVariable "a"]]) (SList [SReturn (SList [SVariable "a", SOperation "*", SVariable "a"])])]

    it "Handles a program with loops and conditionals" $ do
      let input = "tantque [i > 0] (i = i - 1)\nsi [i == 0] (result 42)"
      case parse pProgram "" input of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ast = parseFinalAST sexpr
          case lastCheck ast of
            Left err -> expectationFailure $ "AST validation failed: " ++ err
            Right finalAST -> finalAST `shouldBe` SList [SList [SLoop (SList [SVariable "i", SOperation ">", SInt 0]) (SList [SVariable "i", SOperation "=", SVariable "i", SOperation "-", SInt 1]), SIf (SList [SVariable "i", SOperation "==", SInt 0]) (SList [SReturn (SList [SInt 42])]) (SList [])]]

    it "Handles invalid SExpr gracefully" $ do
      let input = "carre : ent [ent (a)] (result a * a)\n\ncarre : ent [ent (a)] (result a * a)"
      case parse pProgram "" input of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ast = lastCheck(parseFinalAST sexpr)
          ast `shouldBe` Left "ERROR carre is initialized 2 times"
