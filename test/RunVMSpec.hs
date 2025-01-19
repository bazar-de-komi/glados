module RunVMSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Control.Exception (evaluate)

import VM.RunVM (initializeVM, findLabel, execute, executeInstructions, runVM)
import Structure (Value (..), BinaryComparator (..), BinaryOperator (..), Instruction (..), VM (..))

spec :: Spec
spec = do
    describe "VM tests" $ do
        testInitializeVM
        testFindLabel
        testExecute
        testExecuteInstructions
        testRunVM

testInitializeVM :: Spec
testInitializeVM = do
  describe "initializeVM" $ do
    it "initializes a VM with an empty stack and variables" $ do
      let generatedInstructions = [STORE_CONST (VInt 42), HALT]
      initializeVM generatedInstructions `shouldBe` VM { 
          stack = [],
          variables = Map.empty,
          index = 0,
          indexBeforeFuncCall = Nothing,
          instructions = generatedInstructions
      }
    it "initializes a VM with an empty stack, variables and instructions" $ do
      let generatedInstructions = []
      initializeVM generatedInstructions `shouldBe` VM {
          stack = [],
          variables = Map.empty,
          index = 0,
          indexBeforeFuncCall = Nothing,
          instructions = generatedInstructions
      }

testFindLabel :: Spec
testFindLabel = do
  describe "findLabel" $ do
    it "finds the correct index for a label" $ do
      let generatedInstructions = [LABEL "start", STORE_CONST (VInt 42), HALT]
      findLabel "start" generatedInstructions `shouldBe` Just 0

    it "finds the correct index for a function end label" $ do
      let generatedInstructions = [LABEL_FUNC "func", LABEL_FUNC_END "func", STORE_CONST (VInt 42), HALT]
      findLabel "func" generatedInstructions `shouldBe` Just 1

    it "returns Nothing for a non-existent label" $ do
      let generatedInstructions = [STORE_CONST (VInt 42), HALT]
      findLabel "missing" generatedInstructions `shouldBe` Nothing

testExecute :: Spec
testExecute = do
  describe "execute" $ do
    it "executes STORE_CONST to push a value onto the stack" $ do
      let vm = initializeVM []
      let vm' = execute (STORE_CONST (VInt 42)) vm
      stack vm' `shouldBe` [VInt 42]
      index vm' `shouldBe` 1

    it "executes STORE_VAR to store a variable" $ do
      let vm = (initializeVM []) { stack = [VInt 42] }
      let vm' = execute (STORE_VAR "x") vm
      variables vm' `shouldBe` Map.fromList [("x", VInt 42)]
      stack vm' `shouldBe` []
      index vm' `shouldBe` 1

    it "executes LOAD_VAR to load a variable onto the stack" $ do
      let vm = (initializeVM []) { variables = Map.fromList [("x", VInt 42)] }
      let vm' = execute (LOAD_VAR "x") vm
      stack vm' `shouldBe` [VInt 42]
      index vm' `shouldBe` 1

    it "executes OPERATOR ADD for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 1, VInt 2] }
      let vm' = execute (OPERATOR ADD) vm
      stack vm' `shouldBe` [VInt 3]
      index vm' `shouldBe` 1

    it "executes COMPARATOR COMPARE_EQ for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 42, VInt 42] }
      let vm' = execute (COMPARATOR COMPARE_EQ) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes HALT to stop the VM" $ do
      let vm = initializeVM [HALT]
      let vm' = execute HALT vm
      index vm' `shouldBe` -1

testExecuteInstructions :: Spec
testExecuteInstructions = do
  describe "executeInstructions" $ do
    it "executes a sequence of instructions until HALT" $ do
      let generatedInstructions = [STORE_CONST (VInt 42), STORE_VAR "x", LOAD_VAR "x", HALT]
      let vm = initializeVM generatedInstructions
      let vm' = executeInstructions vm
      stack vm' `shouldBe` [VInt 42]
      variables vm' `shouldBe` Map.fromList [("x", VInt 42)]
      index vm' `shouldBe` -1

testRunVM :: Spec
testRunVM = do
  describe "runVM" $ do
    it "runs a VM with simple bytecode" $ do
      let generatedInstructions = [STORE_CONST (VInt 42), HALT]
      let vm = runVM generatedInstructions
      stack vm `shouldBe` [VInt 42]
      index vm `shouldBe` -1
