module StructureSpec (spec) where

import Test.Hspec
import Structure (Value(..), BinaryOperator(..), BinaryComparator(..), Instruction(..), VM(..))
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Value" $ do
    it "should display integer values correctly" $ do
      show (VInt 42) `shouldBe` "42"

    it "should display boolean values correctly" $ do
      show (VBool True) `shouldBe` "True"
      show (VBool False) `shouldBe` "False"

    it "should display floating-point values correctly" $ do
      show (VFloat 3.14) `shouldBe` "3.14"

    it "should display string values correctly" $ do
      show (VString "Hello") `shouldBe` "\"Hello\""

    it "should display character values correctly" $ do
      show (VChar 'A') `shouldBe` "'A'"

  describe "BinaryOperator" $ do
    it "should show all binary operators correctly" $ do
      show ADD `shouldBe` "ADD"
      show SUBTRACT `shouldBe` "SUBTRACT"
      show MULTIPLY `shouldBe` "MULTIPLY"
      show DIVIDE `shouldBe` "DIVIDE"
      show MODULO `shouldBe` "MODULO"

  describe "BinaryComparator" $ do
    it "should show all binary comparators correctly" $ do
      show COMPARE_GT `shouldBe` "COMPARE_GT"
      show COMPARE_LT `shouldBe` "COMPARE_LT"
      show COMPARE_EQ `shouldBe` "COMPARE_EQ"
      show COMPARE_NE `shouldBe` "COMPARE_NE"
      show COMPARE_GE `shouldBe` "COMPARE_GE"
      show COMPARE_LE `shouldBe` "COMPARE_LE"

  describe "Instruction" $ do
    it "should show STORE_CONST correctly" $ do
      show (STORE_CONST (VInt 42)) `shouldBe` "STORE_CONST 42"

    it "should show STORE_VAR correctly" $ do
      show (STORE_VAR "x") `shouldBe` "STORE_VAR \"x\""

    it "should show LOAD_VAR correctly" $ do
      show (LOAD_VAR "y") `shouldBe` "LOAD_VAR \"y\""

    it "should show OPERATOR correctly" $ do
      show (OPERATOR ADD) `shouldBe` "OPERATOR ADD"

    it "should show COMPARATOR correctly" $ do
      show (COMPARATOR COMPARE_EQ) `shouldBe` "COMPARATOR COMPARE_EQ"

    it "should show JUMP correctly" $ do
      show (JUMP "label") `shouldBe` "JUMP \"label\""

    it "should show RETURN correctly" $ do
      show RETURN `shouldBe` "RETURN"

    it "should show HALT correctly" $ do
      show HALT `shouldBe` "HALT"

  describe "VM" $ do
    it "should display the first stack value if present" $ do
      let vm = VM { stack = [VInt 10], variables = Map.empty, index = 0, callStack = [], instructions = [] }
      show vm `shouldBe` "10"

    it "should display a default message if the stack is empty" $ do
      let vm = VM { stack = [], variables = Map.empty, index = 0, callStack = [], instructions = [] }
      show vm `shouldBe` "No return value"

    it "should handle an empty VM state" $ do
      let vm = VM { stack = [], variables = Map.empty, index = 0, callStack = [], instructions = [] }
      stack vm `shouldBe` []
      Map.null (variables vm) `shouldBe` True
      index vm `shouldBe` 0
      callStack vm `shouldBe` []
      instructions vm `shouldBe` []
