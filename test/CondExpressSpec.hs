module CondExpressSpec (spec) where

import Test.Hspec
import Structure (AST(..))
import HandleAST.ConditionalExpressions (condExpress)

spec :: Spec
spec = do
    describe "Conditional Expressions" $ do

        it "evaluates the then branch when condition is true, return 1" $ do
            let env = SList []
            condExpress env (SBool True) (SInt 1) [SInt 2] `shouldBe` Just (SInt 1)

        it "evaluates the else branch when condition is false, return 2" $ do
            let env = SList []
            condExpress env (SBool False) (SInt 1) [SInt 2] `shouldBe` Just (SInt 2)

        it "resolves symbols in condition using define, condition true : return 1" $ do
            let env = SList [SList [SSymbol "define", SSymbol "x", SBool True]]
            condExpress env (SSymbol "x") (SInt 1) [SInt 2] `shouldBe` Just (SInt 1)

        it "Evaluates the then branch when the condition is a less-than comparison" $ do
            let env = SList []
            condExpress env (SList [SSymbol "<", SInt 5, SInt 10]) (SInt 100) [SInt 0]
                `shouldBe` Just (SInt 100)

        it "Evaluates the else branch when the condition is a greater-than comparison" $ do
            let env = SList []
            condExpress env (SList [SSymbol ">", SInt 5, SInt 10]) (SInt 100) [SInt 0]
                `shouldBe` Just (SInt 0)

        it "evaluates nested expressions in the then branch" $ do
            let env = SList []
            condExpress env (SBool True) (SList [SSymbol "+", SInt 3, SInt 4]) [SInt 2]
                `shouldBe` Just (SInt 7)

        -- it "evaluates nested expressions in the else branch" $ do
        --     let env = SList []
        --     condExpress env (SBool False) [SInt 2] [SList [SSymbol "mod", SInt 10, SInt 3]]

        it "returns Nothing for an invalid condition" $ do
            let env = SList []
            condExpress env (SInt 42) (SInt 1) [SInt 2] `shouldBe` Nothing

        it "returns Nothing for an unresolvable symbol in condition" $ do
            let env = SList []
            condExpress env (SSymbol "y") (SInt 1) [SInt 2] `shouldBe` Nothing
    
    -- Test simple boolean conditions
        it "evaluates the `then` branch when condition is true" $ do
            let env = SList []
            condExpress env (SBool True) (SInt 1) [SInt 2] `shouldBe` Just (SInt 1)

        it "evaluates the `else` branch when condition is false" $ do
            let env = SList []
            condExpress env (SBool False) (SInt 1) [SInt 2] `shouldBe` Just (SInt 2)

        -- Test symbol resolution
        it "resolves symbols in condition using `define`" $ do
            let env = SList [SList [SSymbol "define", SSymbol "x", SBool True]]
            condExpress env (SSymbol "x") (SInt 1) [SInt 2] `shouldBe` Just (SInt 1)

        -- Test operators in conditions
        it "handles '<' operator and evaluates the `then` branch when condition is true" $ do
            let env = SList []
            condExpress env (SList [SSymbol "<", SInt 5, SInt 10]) (SInt 100) [SInt 0]
                `shouldBe` Just (SInt 100)

        it "handles '<' operator and evaluates the `else` branch when condition is false" $ do
            let env = SList []
            condExpress env (SList [SSymbol "<", SInt 10, SInt 5]) (SInt 100) [SInt 0]
                `shouldBe` Just (SInt 0)

        it "handles '>' operator and evaluates the `then` branch when condition is true" $ do
            let env = SList []
            condExpress env (SList [SSymbol ">", SInt 10, SInt 5]) (SInt 100) [SInt 0]
                `shouldBe` Just (SInt 100)

        it "handles '>' operator and evaluates the `else` branch when condition is false" $ do
            let env = SList []
            condExpress env (SList [SSymbol ">", SInt 5, SInt 10]) (SInt 100) [SInt 0]
                `shouldBe` Just (SInt 0)

        -- Test nested expressions
        it "evaluates nested arithmetic expressions in the `then` branch" $ do
            let env = SList []
            condExpress env (SBool True) (SList [SSymbol "+", SInt 3, SInt 4]) [SInt 2]
                `shouldBe` Just (SInt 7)

        it "evaluates nested arithmetic expressions in the `else` branch" $ do
            let env = SList []
            condExpress env (SBool False) (SInt 2) [SList [SSymbol "mod", SInt 10, SInt 3]]
                `shouldBe` Just (SInt 1)

        -- Test invalid conditions
        it "returns Nothing for an invalid condition (non-boolean value)" $ do
            let env = SList []
            condExpress env (SInt 42) (SInt 1) [SInt 2] `shouldBe` Nothing

        it "returns Nothing when the condition symbol cannot be resolved" $ do
            let env = SList []
            condExpress env (SSymbol "y") (SInt 1) [SInt 2] `shouldBe` Nothing

        -- Test edge cases
        it "handles a missing `else` branch gracefully" $ do
            let env = SList []
            condExpress env (SBool False) (SInt 1) [] `shouldBe` Nothing

        it "returns Nothing when the condition is a malformed operator" $ do
            let env = SList []
            condExpress env (SList [SSymbol "invalid-op", SInt 5, SInt 10]) (SInt 100) [SInt 0]
                `shouldBe` Nothing
