module ParseSExprSpec (spec) where

import Test.Hspec
import Structure
import Parser.ParserLispSExp

spec :: Spec
spec = do
    describe "Parser Lisp SE" $ do
        it "parse a single atom" $ do
            parseSExpr "x" `shouldBe` Just (Atom "x")

        it "parse a simple list" $ do
            parseSExpr "(x y)" `shouldBe` Just (List [Atom "x", Atom "y"])

        it "parse a nested list" $ do
            parseSExpr "(x (y z))" `shouldBe` Just (List [Atom "x", List [Atom "y", Atom "z"]])

        it "returns Nothing for invalid input" $ do
            parseSExpr "(x y" `shouldBe` Nothing

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
