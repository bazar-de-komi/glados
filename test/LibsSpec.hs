module LibsSpec (spec) where

import Test.Hspec
import Lib (litostr, needParenthese, checkparenthese, checkNotEnd, checkAllString)
import Structure (Value(..))

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

-- testShowValue :: Spec
-- testShowValue = describe "Show Value" $ do

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
          show value `shouldBe` "\"hello\"" -- Les chaînes sont affichées avec des guillemets

        it "displays a character value" $ do
          let value = VChar 'a'
          show value `shouldBe` "'a'" -- Les caractères sont affichés avec des apostrophes

