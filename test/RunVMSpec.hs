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
          callStack = [],
          instructions = generatedInstructions
      }
    it "initializes a VM with an empty stack, variables and instructions" $ do
      let generatedInstructions = []
      initializeVM generatedInstructions `shouldBe` VM {
          stack = [],
          variables = Map.empty,
          index = 0,
          callStack = [],
          instructions = generatedInstructions
      }

testFindLabel :: Spec
testFindLabel = do
  describe "findLabel" $ do
    it "finds the correct index for a function end label" $ do
      let generatedInstructions = [LABEL_FUNC "func", LABEL_FUNC_END "func", STORE_CONST (VInt 42), HALT]
      findLabel (LABEL_FUNC "func") generatedInstructions `shouldBe` Just 1

    it "finds the correct index for a CALL instruction" $ do
      let generatedInstructions = [CALL "func", LABEL_FUNC "func", LABEL_FUNC_END "func", STORE_CONST (VInt 42), HALT]
      findLabel (CALL "func") generatedInstructions `shouldBe` Just 1

    it "finds the correct index for a JUMP instruction" $ do
      let generatedInstructions = [JUMP "label", LABEL "label", STORE_CONST (VInt 42), HALT]
      findLabel (JUMP "label") generatedInstructions `shouldBe` Just 1

    it "finds the correct index for a JUMP_IF_FALSE instruction" $ do
      let generatedInstructions = [JUMP_IF_FALSE "label", LABEL "label", STORE_CONST (VInt 42), HALT]
      findLabel (JUMP_IF_FALSE "label") generatedInstructions `shouldBe` Just 1

    it "returns Nothing for a non-existent function end label" $ do
      let generatedInstructions = [LABEL_FUNC "func", STORE_CONST (VInt 42), HALT]
      findLabel (LABEL_FUNC "func") generatedInstructions `shouldBe` Nothing

    it "returns Nothing for a non-existent CALL instruction" $ do
      let generatedInstructions = [STORE_CONST (VInt 42), HALT]
      findLabel (CALL "missing") generatedInstructions `shouldBe` Nothing

    it "returns Nothing for a non-existent JUMP instruction" $ do
      let generatedInstructions = [STORE_CONST (VInt 42), HALT]
      findLabel (JUMP "missing") generatedInstructions `shouldBe` Nothing

    it "returns Nothing for a non-existent JUMP_IF_FALSE instruction" $ do
      let generatedInstructions = [STORE_CONST (VInt 42), HALT]
      findLabel (JUMP_IF_FALSE "missing") generatedInstructions `shouldBe` Nothing

    it "returns Nothing for a other instruction" $ do
      let generatedInstructions = [STORE_CONST (VInt 42), HALT]
      findLabel (STORE_VAR "missing") generatedInstructions `shouldBe` Nothing

testExecute :: Spec
testExecute = do
  describe "execute" $ do
    describe "STORE_CONST" $ do
      it "pushes a value onto the stack" $ do
        let vm = initializeVM []
        let vm' = execute (STORE_CONST (VInt 42)) vm
        stack vm' `shouldBe` [VInt 42]
        index vm' `shouldBe` 1

    describe "STORE_VAR" $ do
      it "stores a variable from the stack" $ do
        let vm = (initializeVM []) { stack = [VInt 42] }
        let vm' = execute (STORE_VAR "x") vm
        variables vm' `shouldBe` Map.fromList [("x", VInt 42)]
        stack vm' `shouldBe` []
        index vm' `shouldBe` 1

      it "throws an error on empty stack" $ do
        let vm = initializeVM []
        evaluate (execute (STORE_VAR "x") vm) `shouldThrow` anyErrorCall

    describe "LOAD_VAR" $ do
      it "loads a variable onto the stack" $ do
        let vm = (initializeVM []) { variables = Map.fromList [("x", VInt 42)] }
        let vm' = execute (LOAD_VAR "x") vm
        stack vm' `shouldBe` [VInt 42]
        index vm' `shouldBe` 1

      it "throws an error for undefined variables" $ do
        let vm = initializeVM []
        evaluate (execute (LOAD_VAR "x") vm) `shouldThrow` anyErrorCall

    describe "OPERATOR" $ do
      it "executes ADD for integers" $ do
        let vm = (initializeVM []) { stack = [VInt 1, VInt 2] }
        let vm' = execute (OPERATOR ADD) vm
        stack vm' `shouldBe` [VInt 3]
        index vm' `shouldBe` 1

      it "executes ADD for floats" $ do
        let vm = (initializeVM []) { stack = [VFloat 1.5, VFloat 2.5] }
        let vm' = execute (OPERATOR ADD) vm
        stack vm' `shouldBe` [VFloat 4.0]
        index vm' `shouldBe` 1

      it "throws an error for stack underflow" $ do
        let vm = initializeVM []
        evaluate (execute (OPERATOR ADD) vm) `shouldThrow` anyErrorCall

    describe "COMPARATOR" $ do
      it "executes COMPARE_EQ for integers" $ do
        let vm = (initializeVM []) { stack = [VInt 42, VInt 42] }
        let vm' = execute (COMPARATOR COMPARE_EQ) vm
        stack vm' `shouldBe` [VBool True]
        index vm' `shouldBe` 1

      it "executes COMPARE_GT for floats" $ do
        let vm = (initializeVM []) { stack = [VFloat 1.5, VFloat 2.5] }
        let vm' = execute (COMPARATOR COMPARE_GT) vm
        stack vm' `shouldBe` [VBool True]
        index vm' `shouldBe` 1

      it "throws an error for stack underflow" $ do
        let vm = initializeVM []
        evaluate (execute (COMPARATOR COMPARE_EQ) vm) `shouldThrow` anyErrorCall

    describe "JUMP" $ do
      it "jumps to a label" $ do
        let generatedInstructions = [LABEL "start", STORE_CONST (VInt 42), JUMP "start"]
        let vm = initializeVM generatedInstructions
        let vm' = execute (JUMP "start") vm
        index vm' `shouldBe` 0

      it "throws an error if the label is not found" $ do
        let vm = initializeVM []
        evaluate (execute (JUMP "unknown") vm) `shouldThrow` anyErrorCall

    describe "JUMP_IF_FALSE" $ do
      it "jumps if the top of the stack is False" $ do
        let generatedInstructions = [LABEL "skip"]
        let vm = (initializeVM generatedInstructions) { stack = [VBool False] }
        let vm' = execute (JUMP_IF_FALSE "skip") vm
        index vm' `shouldBe` 0
        stack vm' `shouldBe` []

      it "does not jump if the top of the stack is True" $ do
        let generatedInstructions = [LABEL "skip"]
        let vm = (initializeVM generatedInstructions) { stack = [VBool True] }
        let vm' = execute (JUMP_IF_FALSE "skip") vm
        index vm' `shouldBe` 1
        stack vm' `shouldBe` []

      it "throws an error for stack underflow" $ do
        let vm = initializeVM []
        evaluate (execute (JUMP_IF_FALSE "label") vm) `shouldThrow` anyErrorCall

    describe "CALL" $ do
      it "calls a function by jumping to its label" $ do
        let generatedInstructions = [LABEL "func", RETURN, LABEL_FUNC "func"]
        let vm = initializeVM generatedInstructions
        let vm' = execute (CALL "func") vm
        index vm' `shouldBe` 2
        callStack vm' `shouldBe` [1]

      it "throws an error if the function label is not found" $ do
        let vm = initializeVM []
        evaluate (execute (CALL "unknown") vm) `shouldThrow` anyErrorCall

    describe "HALT" $ do
      it "stops the VM" $ do
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

    it "runs a VM with complex function call bytecode" $ do
      let generatedInstructions =
              [ LABEL_FUNC "main"
              , STORE_CONST (VInt 10)
              , CALL "addOne"
              , RETURN
              , LABEL_FUNC_END "main"
              , LABEL_FUNC "addOne"
              , STORE_CONST (VInt 1)
              , OPERATOR ADD
              , RETURN
              , LABEL_FUNC_END "addOne"
              , CALL "main"
              ]
      let vm = runVM generatedInstructions
      stack vm `shouldBe` [VInt 11]
