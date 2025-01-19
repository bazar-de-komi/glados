module ParserSExpASTSpec (spec) where

import Test.Hspec
import Parser.ParserSExpAST (parseFinalAST, checkFinalAST, lastCheck)
import Structure (SExpr(..), AST(..))
import Parser.ParserKleftisSExp (pProgram)
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "Functional Tests for Parser.ParserSExpAST" $ do

    it "Parses and processes a simple program with definitions" $ do
      let input = "(def x ent (42))\n\ndef y reel (3.14)\n\npour (= i 0) [i < 10] (i += 1) (print i)"
      case parse pProgram "" input of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ast = parseFinalAST sexpr
          case lastCheck ast of
            Left err -> expectationFailure $ "AST validation failed: " ++ err
            Right finalAST -> finalAST `shouldBe` SList
              [ SDefine "x" (SType "int") (SList [SInt 42])
              , SDefine "y" (SType "float") (SList [SFloat 3.14])
              , SFor (SList [SOperation "=", SVariable "i", SInt 0])
                     (SList [SVariable "i", SOperation "<", SInt 10])
                     (SList [SOperation "+=", SVariable "i", SInt 1])
                     (SList [SVariable "print", SVariable "i"])
              ]

    it "Processes a program with a function definition and a conditional statement" $ do
      let input = "(def add ent [a b] (a + b))\n\nsi [x > y] (résult x) sinon (résult y)"
      case parse pProgram "" input of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ast = parseFinalAST sexpr
          case lastCheck ast of
            Left err -> expectationFailure $ "AST validation failed: " ++ err
            Right finalAST -> finalAST `shouldBe` SList
              [ SFunc "add" (SType "int")
                     (SList [SVariable "a", SVariable "b"])
                     (SList [SOperation "+", SVariable "a", SVariable "b"])
              , SIf (SList [SVariable "x", SOperation ">", SVariable "y"])
                    (SList [SVariable "result", SVariable "x"])
                    (SList [SVariable "result", SVariable "y"])
              ]

    it "Handles errors in the AST validation (duplicate definitions)" $ do
      let input = "(def x ent (42))\n\ndef x reel (3.14)"
      case parse pProgram "" input of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ast = parseFinalAST sexpr
          case lastCheck ast of
            Left err -> err `shouldContain` "x is initialized 2 times"
            Right _ -> expectationFailure "Validation should have failed due to duplicate definitions"

    it "Processes a program with nested loops and multiple expressions" $ do
      let input = "pour (= i 0) [i < 10] (i += 1) (\n tantque [j < i] (j += 1)\n (print j))"
      case parse pProgram "" input of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ast = parseFinalAST sexpr
          case lastCheck ast of
            Left err -> expectationFailure $ "AST validation failed: " ++ err
            Right finalAST -> finalAST `shouldBe` SList
              [ SFor (SList [SOperation "=", SVariable "i", SInt 0])
                     (SList [SVariable "i", SOperation "<", SInt 10])
                     (SList [SOperation "+=", SVariable "i", SInt 1])
                     (SList
                        [ SLoop (SList [SVariable "j", SOperation "<", SVariable "i"])
                                (SList [SOperation "+=", SVariable "j", SInt 1])
                        , SList [SVariable "print", SVariable "j"]
                        ])
              ]

    it "Handles unsupported expressions with proper error messages" $ do
      let input = "(unsupported_expr)"
      case parse pProgram "" input of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ast = parseFinalAST sexpr
          ast `shouldBe` Left "Unsupported SExpr: (unsupported_expr)"
