module StructureSpec (spec) where

import Test.Hspec
import Structure (SExpr(..), AST(..))

spec :: Spec
spec = do
    describe "Structure SExpress - Basic Test" $ do
        it "should show Atom correctly" $ do
            show (Atom "x") `shouldBe` "x"

        it "compare two equal atomic SExpr values" $ do
            Atom "x" `shouldBe` Atom "x"

        it "compare two different atomic SExpr values" $ do
            Atom "x" `shouldNotBe` Atom "y"

        it "should show List correctly" $ do
            show (List [Atom "x", Atom "y"]) `shouldBe` "(x y)"

        it "shows a List of Atoms correctly" $ do
            show (List [Atom "x", Atom "y", Atom "z"]) `shouldBe` "(x y z)"

        it "shows an empty List correctly" $ do
            show (List []) `shouldBe` "()"

        it "compares two equal list SExpr values" $ do
            List [Atom "x", Atom "y"] `shouldBe` List [Atom "x", Atom "y"]

        it "compares two different list SExpr values" $ do
            List [Atom "x", Atom "y"] `shouldNotBe` List [Atom "x", Atom "z"]

    describe "Structure SExpress - Complex Cases" $ do
        it "shows a deeply nested List correctly" $ do
            show (List [List [List [Atom "a"], Atom "b"], Atom "c"]) `shouldBe` "(((a) b) c)"

        it "should handle nested lists correctly" $ do
            show (List [Atom "x", List [Atom "y", Atom "z"]]) `shouldBe` "(x (y z))"

        it "should handle an empty list" $ do
            show (List []) `shouldBe` "()"

        it "compares two empty list SExpr values" $ do
            List [] `shouldBe` List []

        it "compares two deeply nested AST values" $ do
            SList [SInt 1, SList [SSymbol "foo", SBool True]] `shouldBe` SList [SInt 1, SList [SSymbol "foo", SBool True]]

        it "compares two nested list SExpr values" $ do
            List [Atom "x", List [Atom "y", Atom "z"]] `shouldBe` List [Atom "x", List [Atom "y", Atom "z"]]


    describe "Structure AST - Basic Construction" $ do
        it "create a SInt" $ do
            SInt 42 `shouldBe` SInt 42

        it "create a SList" $ do
            SList [SInt 1, SInt 2] `shouldBe` SList [SInt 1, SInt 2]

        it "create a SSymbol " $ do
            SSymbol "foo" `shouldBe` SSymbol "foo"

        it "create a SBool True" $ do
            SBool True `shouldBe` SBool True

        it "create a SBool False" $ do
            SBool False `shouldBe` SBool False

        it "shows an int AST value" $ do
            show (SInt 42) `shouldBe` "int : 42"

        it "shows a symbolic AST value" $ do
            show (SSymbol "foo") `shouldBe` "str : foo"

        it "shows a boolean AST value (True)" $ do
            show (SBool True) `shouldBe` "bool : #t"

        it "shows a boolean AST value (False)" $ do
            show (SBool False) `shouldBe` "bool : #f"

        it "shows a list AST value" $ do
            show (SList [SInt 42, SBool False]) `shouldBe` "(int : 42 bool : #f)"

        it "shows a list of ASTs correctly" $ do
            show (SList [SInt 1, SBool True, SSymbol "x"]) `shouldBe` "(int : 1 bool : #t str : x)"

        it "shows a nested list of ASTs correctly" $ do
            show (SList [SInt 1, SList [SSymbol "foo", SBool False]]) `shouldBe` "(int : 1 (str : foo bool : #f))"

        it "shows an empty list AST value" $ do
            show (SList []) `shouldBe` "()"

        it "compares two different integer AST values" $ do
            SInt 42 `shouldNotBe` SInt 43

        it "compares two different symbolic AST values" $ do
            SSymbol "foo" `shouldNotBe` SSymbol "bar"

        it "compares two different boolean AST values" $ do
            SBool True `shouldNotBe` SBool False

        it "compares two equal list AST with Sint and Sbool values" $ do
            SList [SInt 1, SBool True] `shouldBe` SList [SInt 1, SBool True]

        it "compares two empty list AST values" $ do
            SList [] `shouldBe` SList []

        it "compares two deeply nested AST values" $ do
            SList [SInt 1, SList [SSymbol "foo", SBool True]] `shouldBe` SList [SInt 1, SList [SSymbol "foo", SBool True]]
