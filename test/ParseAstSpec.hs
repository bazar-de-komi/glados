module ParseAstSpec (spec) where

import Test.Hspec
import Structure (AST(..))
import Parser.ParserSExpAST (isInt, isBool)

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
        
    describe "Utility Functions" $ do
        it "identifies integers with isInt" $ do
            isInt "123" `shouldBe` True
            isInt "42" `shouldBe` True
            isInt "abc" `shouldBe` False

        it "identifies booleans with isBool" $ do
            isBool "#t" `shouldBe` True
            isBool "#f" `shouldBe` True
            isBool "#x" `shouldBe` False
            isBool "true" `shouldBe` False