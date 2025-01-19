module LibsSpec (spec) where

import Test.Hspec
import Lib (litostr, needParenthese, checkparenthese, checkNotEnd, checkAllString, backToFile)
import Structure (Value(..), LabelState(..))

spec:: Spec
spec = do
    describe "check parenthese" $ do
        it "add spaces before parentheses" $ do
            checkparenthese "(define x 42)" `shouldBe` " ( define x 42 ) "

        it "does nothing if there are no parentheses" $ do
            checkparenthese "define x 42" `shouldBe` "define x 42"
        
        it "adds spaces around parentheses" $ do
            checkparenthese "(a)" `shouldBe` " ( a ) "

        it "adds spaces around brackets" $ do
          checkparenthese "[a]" `shouldBe` " [ a ] "

        it "adds spaces around braces" $ do
          checkparenthese "{a}" `shouldBe` " { a } "

        it "returns an empty string when given an empty string" $ do
          checkparenthese "" `shouldBe` ""

        it "does not add spaces around other characters" $ do
          checkparenthese "abc123" `shouldBe` "abc123"

    describe "convert list to str" $ do
        it "converts a list of strings into a single string with spaces" $ do
            litostr ["define", "x", "(42)"] `shouldBe` "define\nx\n(42)"

        it "returns an empty string for an empty list" $ do
            litostr [] `shouldBe` ""

    describe "check list not end by space" $ do
        it "return True for a string with non-space characters" $ do
            checkNotEnd "     x" `shouldBe` True

        it "return False for s string with only spaces" $ do
            checkNotEnd "    " `shouldBe` False

    describe "check parenthese are stable" $ do
        it "returns True for balanced parentheses" $ do
            checkAllString "(define (x))" 0 `shouldBe` False

        it "returns False for unbalanced parentheses" $ do
            checkAllString "(define (x)" 0 `shouldBe` False

        it "handles nested parentheses correctly" $ do
            checkAllString "((a (b)))" 0 `shouldBe` False

    describe "parenthese are add" $ do
        it "adds parentheses to unbalanced strings" $ do
            needParenthese "lambda (a b) (+ a b)" `shouldBe` "(lambda (a b) (+ a b))"

        it "does not modify, already balanced strings" $ do
            needParenthese "(lambda (a b) (+ a b))" `shouldBe` "(lambda (a b) (+ a b))"

        it "displays an integer value" $ do
          let value = VInt 42
          show value `shouldBe` "42"

        it "displays a float value" $ do
          let value = VFloat 3.14
          show value `shouldBe` "3.14"

        it "displays a boolean value (True)" $ do
          let value = VBool True
          show value `shouldBe` "True"

        it "displays a boolean value (False)" $ do
          let value = VBool False
          show value `shouldBe` "False"

        it "displays a string value" $ do
          let value = VString "hello"
          show value `shouldBe` "\"hello\""

        it "displays a character value" $ do
          let value = VChar 'a'
          show value `shouldBe` "'a'"

        it "correctly creates a LabelState with initialized counters" $ do
          let state = LabelState 0 0
          loopCounter state `shouldBe` 0
          ifCounter state `shouldBe` 0

        it "to modify the loopCounter" $ do
          let state = LabelState 1 0
          loopCounter state `shouldBe` 1
          ifCounter state `shouldBe` 0

        it "is used to modify the ifCounter" $ do
          let state = LabelState 0 2
          loopCounter state `shouldBe` 0
          ifCounter state `shouldBe` 2

        it "to modify both counters" $ do
          let state = LabelState 3 4
          loopCounter state `shouldBe` 3
          ifCounter state `shouldBe` 4

        it "creates a LabelState with negative values" $ do
          let state = LabelState (-1) (-2)
          loopCounter state `shouldBe` (-1)
          ifCounter state `shouldBe` (-2)

        it "returns an empty string for an empty list" $ do
            backToFile [] `shouldBe` ""

        it "returns the single element for a one-element list" $ do
            backToFile ["Hello"] `shouldBe` "Hello\n"

        it "concatenates multiple elements with \\n" $ do
            backToFile ["Hello", "World", "Test"] `shouldBe` "Hello\nWorld\nTest\n"

        it "handles a list with empty strings correctly" $ do
            backToFile ["Hello", "", "World"] `shouldBe` "Hello\n\nWorld\n"

        it "handles a list of empty strings" $ do
            backToFile ["", "", ""] `shouldBe` "\n\n\n"

        it "handles a list with mixed empty and non-empty strings" $ do
            backToFile ["First", "", "Last"] `shouldBe` "First\n\nLast\n"
