module LibsSpec (spec) where

import Test.Hspec
import Lib (litostr, needParenthese, checkparenthese, checkNotEnd, checkAllString)
import Structure (Value(..), LabelState(..), Instruction(..), VM(..))
import qualified Data.Map as Map

spec:: Spec
spec = do
    describe "check parenthese" $ do
        it "add spaces before parentheses" $ do
            checkparenthese "(define x 42)" `shouldBe` " ( define x 42 ) "

        it "does nothing if there are no parentheses" $ do
            checkparenthese "define x 42" `shouldBe` "define x 42"

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

        it "creates a VM with an empty stack and variables" $ do
          let vm = VM [] Map.empty 0 Nothing []
          stack vm `shouldBe` []
          variables vm `shouldBe` Map.empty
          index vm `shouldBe` 0
          indexBeforeFuncCall vm `shouldBe` Nothing
          instructions vm `shouldBe` []

        it "allows adding values to the stack" $ do
          let vm = VM [VInt 1, VBool True] Map.empty 0 Nothing []
          stack vm `shouldBe` [VInt 1, VBool True]

        it "allows storing variables" $ do
          let vars = Map.fromList [("x", VInt 10), ("y", VFloat 3.14)]
          let vm = VM [] vars 0 Nothing []
          variables vm `shouldBe` vars

        it "allows setting the instruction index" $ do
          let vm = VM [] Map.empty 5 Nothing []
          index vm `shouldBe` 5

        it "allows setting the index before function call" $ do
          let vm = VM [] Map.empty 0 (Just 3) []
          indexBeforeFuncCall vm `shouldBe` Just 3

        it "allows loading instructions" $ do
          let instrs = [STORE_CONST (VInt 1), STORE_VAR "x"]
          let vm = VM [] Map.empty 0 Nothing instrs
          instructions vm `shouldBe` instrs

        it "creates a fully populated VM" $ do
            let launch = [VInt 1, VBool False]
            let vars = Map.fromList [("x", VInt 10)]
            let instrs = [STORE_CONST (VInt 1), STORE_VAR "x"]
            let vm = VM launch vars 2 (Just 1) instrs
            stack vm `shouldBe` launch
            variables vm `shouldBe` vars
            index vm `shouldBe` 2
            indexBeforeFuncCall vm `shouldBe` Just 1
            instructions vm `shouldBe` instrs

        it "displays the first value of a non-empty stack" $ do
          let vm = VM [VInt 42, VBool True] Map.empty 0 Nothing []
          show vm `shouldBe` "42"          
        it "displays a float value correctly" $ do
          let vm = VM [VFloat 3.14, VInt 1] Map.empty 0 Nothing []
          show vm `shouldBe` "3.14"        
        it "displays a boolean value correctly" $ do
          let vm = VM [VBool False, VInt 1] Map.empty 0 Nothing []
          show vm `shouldBe` "False"           
        it "displays a string value correctly" $ do
          let vm = VM [VString "Hello", VInt 1] Map.empty 0 Nothing []
          show vm `shouldBe` "\"Hello\""           
        it "displays a char value correctly" $ do
          let vm = VM [VChar 'A', VInt 1] Map.empty 0 Nothing []
          show vm `shouldBe` "'A'"         
        it "displays 'No return value' for an empty stack" $ do
          let vm = VM [] Map.empty 0 Nothing []
          show vm `shouldBe` "No return value"
