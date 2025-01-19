module VMSpec (spec) where

import Test.Hspec
import VM.RunVM (initializeVM, execute, executeInstructions, runVM)
import Control.Exception (evaluate)
import Structure (Value(..), BinaryOperator(..), BinaryComparator(..), Instruction(..), VM(..))
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "VM execution" $ do
    it "should push and pop values on the stack correctly" $ do
      let vm = (initializeVM [STORE_CONST (VInt 10), STORE_CONST (VInt 20)]) { stack = [] }
      let vm1 = execute (STORE_CONST (VInt 10)) vm
      stack vm1 `shouldBe` [VInt 10]
      let vm2 = execute (STORE_CONST (VInt 20)) vm1
      stack vm2 `shouldBe` [VInt 20, VInt 10]

    it "should store and retrieve variables correctly" $ do
      let vm = (initializeVM [STORE_VAR "a", LOAD_VAR "a"]) { stack = [VInt 100] }
      let vm1 = execute (STORE_VAR "a") vm
      variables vm1 `shouldBe` Map.singleton "a" (VInt 100)
      let vm2 = execute (LOAD_VAR "a") vm1
      stack vm2 `shouldBe` [VInt 100]

    it "should perform addition correctly" $ do
      let vm = (initializeVM [OPERATOR ADD]) { stack = [VInt 4, VInt 6] }
      let vm1 = execute (OPERATOR ADD) vm
      stack vm1 `shouldBe` [VInt 10]

    it "should perform subtraction correctly" $ do
      let vm = (initializeVM [OPERATOR SUBTRACT]) { stack = [VInt 3, VInt 7] }
      let vm1 = execute (OPERATOR SUBTRACT) vm
      stack vm1 `shouldBe` [VInt 4]

    it "should perform multiplication correctly" $ do
      let vm = (initializeVM [OPERATOR MULTIPLY]) { stack = [VInt 3, VInt 5] }
      let vm1 = execute (OPERATOR MULTIPLY) vm
      stack vm1 `shouldBe` [VInt 15]

    it "should perform division correctly" $ do
      let vm = (initializeVM [OPERATOR DIVIDE]) { stack = [VInt 3, VInt 12] }
      let vm1 = execute (OPERATOR DIVIDE) vm
      stack vm1 `shouldBe` [VInt 4]

    it "throws an error for division by zero" $ do
      let vm = initializeVM [STORE_CONST (VInt 1), STORE_CONST (VInt 0)]
      evaluate (execute (OPERATOR DIVIDE) vm) `shouldThrow` anyErrorCall

    it "should compare integers correctly" $ do
      let vm = (initializeVM [COMPARATOR COMPARE_GT]) { stack = [VInt 2, VInt 5] }
      let vm1 = execute (COMPARATOR COMPARE_GT) vm
      stack vm1 `shouldBe` [VBool True]

    it "should compare strings correctly" $ do
      let vm = (initializeVM [COMPARATOR COMPARE_EQ]) { stack = [VString "hello", VString "hello"] }
      let vm1 = execute (COMPARATOR COMPARE_EQ) vm
      stack vm1 `shouldBe` [VBool True]

    it "should jump to a label correctly" $ do
      let vm = initializeVM
            [ LABEL "start"
            , STORE_CONST (VInt 42)
            , JUMP "end"
            , LABEL "end"
            , HALT]
      let vm1 = executeInstructions vm
      index vm1 `shouldBe` -1
      stack vm1 `shouldBe` [VInt 42]

    it "should perform conditional jumps correctly" $ do
      let vm = initializeVM
            [ STORE_CONST (VBool False)
            , JUMP_IF_FALSE "skip"
            , STORE_CONST (VInt 99)
            , LABEL "skip"
            , HALT]
      let vm1 = executeInstructions vm
      stack vm1 `shouldBe` []

    it "should call and return from functions correctly" $ do
      let instructions =
            [ CALL "main"
            , LABEL_FUNC "main"
            , STORE_CONST (VInt 10)
            , CALL "addOne"
            , RETURN
            , LABEL_FUNC_END "main"
            , LABEL_FUNC "addOne"
            , STORE_CONST (VInt 1)
            , OPERATOR ADD
            , RETURN
            , LABEL_FUNC_END "addOne"]
      let vm = initializeVM instructions
      let vm1 = executeInstructions vm
      stack vm1 `shouldBe` [VInt 11]

    it "should handle a complex sequence of instructions" $ do
      let instructions =
              [ STORE_CONST (VInt 5)
              , STORE_CONST (VInt 3)
              , OPERATOR MULTIPLY
              , STORE_VAR "result"
              , LOAD_VAR "result"
              , STORE_CONST (VInt 5)
              , OPERATOR ADD
              , HALT
              ]
      let vm = initializeVM instructions
      let vm1 = executeInstructions vm
      stack vm1 `shouldBe` [VInt 20]
      variables vm1 `shouldBe` Map.singleton "result" (VInt 15)

    it "should halt execution when HALT is reached" $ do
      let vm = initializeVM [STORE_CONST (VInt 42), HALT, STORE_CONST (VInt 99)]
      let vm1 = executeInstructions vm
      stack vm1 `shouldBe` [VInt 42]

    it "throws an error for invalid LOAD_VAR" $ do
      let vm = initializeVM [LOAD_VAR "missing"]
      evaluate (execute (LOAD_VAR "missing") vm) `shouldThrow` anyErrorCall

    it "throws an error for RETURN outside of function" $ do
      let vm = initializeVM [RETURN]
      evaluate (execute RETURN vm) `shouldThrow` anyErrorCall
