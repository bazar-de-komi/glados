module LibsSpec (spec) where

import Test.Hspec
import Lib (checkArgs, litostr, needParenthese, checkFlag, tailOf, whilegetline, checkparenthese, checkNotEnd, checkAllString)

spec:: Spec
spec = do
    describe "checkFlag" $ do
        it "return True when -i is in the arguments" $ do
            checkFlag ["-i", "file.scm"] `shouldBe` True

        it "return False when -i is in not in the arguments" $ do
            checkFlag ["file.scm"] `shouldBe` False

    describe "tailOf" $ do
        it "return the last element of a non-empty list" $ do
            tailOf ["first", "seconde", "last"] `shouldBe` "last"

        it "return an empty string for an empty list" $ do
            tailOf [] `shouldBe` ""
    
    describe "check parenthese" $ do
        it "add spaces before parentheses" $ do
            checkparenthese "(define x 42)" `shouldBe` " (define x 42)"
        
        it "does nothing if there are no parentheses" $ do
            checkparenthese "define x 42" `shouldBe` "define x 42"

    describe "convert list to str" $ do
        it "converts a list of strings into a single string with spaces" $ do
            litostr ["define", "x", "(42)"] `shouldBe` "define x  (42) "

        it "returns an empty string for an empty list" $ do
            litostr [] `shouldBe` ""

    describe "check list not end by space" $ do
        it "return True for a string with non-space characters" $ do
            checkNotEnd "     x" `shouldBe` True

        it "return False for s string with only spaces" $ do
            checkNotEnd "    " `shouldBe` False

    describe "check parenthese are stable" $ do
        -- it "returns True for balanced parentheses" $ do
        --     checkAllString "(define (x))" 0 `shouldBe` True

        it "returns False for unbalanced parentheses" $ do
            checkAllString "(define (x)" 0 `shouldBe` False

        -- it "handles nested parentheses correctly" $ do
        --     checkAllString "((a (b)))" 0 `shouldBe` True

    describe "parenthese are add" $ do
        -- it "adds parentheses to unbalanced strings" $ do
        --     needParenthese "define x" `shouldBe` "(define x)"

        it "does not modify already balanced strings" $ do
            needParenthese "(define x)" `shouldBe` "(define x)"