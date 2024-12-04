module GetValueSpec (spec) where

import Test.Hspec
import GetValue.GetValue
import StructureAST.StructureAST (AST (..))

spec :: Spec

spec = do
    
    describe "getValue Basic Test" $ do
        it "returns Nothing for empty list" $ do
            getValue (SList []) (SSymbol "x") `shouldBe` Nothing

        it "returns the value for a matching define binding" $ do
            let input = SList [SList [SSymbol "define", SSymbol "x", SInt 42]]
            getValue input (SSymbol "x") `shouldBe` Just (SInt 42)

        it "returns Nothing if no matching define binding exists" $ do
            let input = SList [SList [SSymbol "define", SSymbol "y", SInt 42]]
            getValue input (SSymbol "x") `shouldBe` Nothing

    describe "getValue Advanced Test" $ do
        it "returns the first matching value in a list of multiple bindings" $ do
            let input = SList
                    [ SList [SSymbol "define", SSymbol "x", SInt 42]
                    , SList [SSymbol "define", SSymbol "x", SInt 99]
                    ]
            getValue input (SSymbol "x") `shouldBe` Just (SInt 42)

        it "ignores non-define nodes" $ do
            let input = SList
                    [ SList [SSymbol "define", SSymbol "x", SInt 42]
                    , SList [SSymbol "+", SInt 1, SInt 2]
                    , SList [SSymbol "define", SSymbol "y", SInt 99]
                    ]
            getValue input (SSymbol "y") `shouldBe` Just (SInt 99)

        it "returns Nothing if the target symbol is not found in a mixed list" $ do
            let input = SList
                    [ SList [SSymbol "+", SInt 1, SInt 2]
                    , SList [SSymbol "define", SSymbol "y", SInt 99]
                    ]
            getValue input (SSymbol "x") `shouldBe` Nothing