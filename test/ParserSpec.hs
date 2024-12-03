module ParserSpec (spec) where

import Test.Hspec
import StructureSE.StructureSE
import Parser_LISP_SE.Parserlispsexp

spec :: Spec
spec = do
    describe "Example test" $ do
        it "should pass" $ do
            True `shouldBe` True

    describe "StructureSe" $ do
        it "should show Atom correctly" $ do
            show (Atom "x") `shouldBe` "x"

        it "should show List correctly" $ do
            show (List [Atom "x", Atom "y"]) `shouldBe` "(x y)"
            
    describe "Parser Lisp SE" $ do
        it "parse a single atom" $ do
            parseSExpr "x" `shouldBe` Just (Atom "x")
        
        it "parse a simple list" $ do
            parseSExpr "(x y)" `shouldBe` Just (List [Atom "x", Atom "y"])

        it "parse a nested list" $ do
            parseSExpr "(x (y z))" `shouldBe` Just (List [Atom "x", List [Atom "y", Atom "z"]])

        it "returns Nothing for invalid input" $ do
            parseSExpr "(x y" `shouldBe` Nothing

    describe "StructureSE - Complex Cases" $ do
        it "should handle nested lists correctly" $ do
            show (List [Atom "x", List [Atom "y", Atom "z"]]) `shouldBe` "(x (y z))"

        it "should handle an empty list" $ do
            show (List []) `shouldBe` "()"

    describe "Parser Lisp SE - Advanced Parsing" $ do
        it "parses a deeply nested list" $ do
            parseSExpr "(a (b (c d)))" `shouldBe` Just (List [Atom "a", List [Atom "b", List [Atom "c", Atom "d"]]])

        it "ignores extra spaces" $ do
            parseSExpr "  (  x   y  ) " `shouldBe` Just (List [Atom "x", Atom "y"])

        it "parses an empty list" $ do
            parseSExpr "()" `shouldBe` Just (List [])

    describe "Parser Lisp SE - Integration with SExpr" $ do
        it "produces a valid nested SExpr from a string" $ do
            parseSExpr "(define x (list 1 2 3))" `shouldBe`
                Just (List [Atom "define", Atom "x", List [Atom "list", Atom "1", Atom "2", Atom "3"]])