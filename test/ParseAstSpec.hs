module ParseAstSpec (spec) where

import Test.Hspec
import StructureAST.StructureAST (AST(..))

spec :: Spec
spec = do
    
    describe "StructureAST - Basic Construction" $ do
        it "create a SInt" $ do
            SInt 42 `shouldBe` SInt 42

        it "create a SList" $ do
            SList [SInt 1, SInt 2] `shouldBe` SList [SInt 1, SInt 2]

    describe "AST Show Instance" $ do
        it "displays a SInt correctly" $ do
            show (SInt 42) `shouldBe` "int : 42"

        it "displays a SSymbol correctly" $ do
            show (SSymbol "hello world") `shouldBe` "str : hello world"

        it "displays a SBool correctly" $ do
            show (SBool True) `shouldBe` "bool : #t"
            show (SBool False) `shouldBe` "bool : #f"

        it "displays a SList correctly" $ do
            show (SList [SInt 1, SSymbol "x", SBool True]) `shouldBe` "(int : 1 str : x bool : #t)"