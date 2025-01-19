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

    it "executes SUBTRACT for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 5, VInt 3] }
      let vm' = execute (OPERATOR SUBTRACT) vm
      stack vm' `shouldBe` [VInt (-2)]
      index vm' `shouldBe` 1

    it "executes MULTIPLY for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 4, VInt 3] }
      let vm' = execute (OPERATOR MULTIPLY) vm
      stack vm' `shouldBe` [VInt 12]
      index vm' `shouldBe` 1

    it "executes DIVIDE for integers 1" $ do
      let vm = (initializeVM []) { stack = [VInt 6, VInt 2] }
      let vm' = execute (OPERATOR DIVIDE) vm
      stack vm' `shouldBe` [VInt 0]
      index vm' `shouldBe` 1

    it "executes DIVIDE for integers 2" $ do
      let vm = (initializeVM []) { stack = [VInt 2, VInt 6] }
      let vm' = execute (OPERATOR DIVIDE) vm
      stack vm' `shouldBe` [VInt 3]
      index vm' `shouldBe` 1

    it "executes MODULO for integers 2" $ do
      let vm = (initializeVM []) { stack = [VInt 2, VInt 6] }
      let vm' = execute (OPERATOR MODULO) vm
      stack vm' `shouldBe` [VInt 0]
      index vm' `shouldBe` 1

    it "executes ADD for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 1.5, VFloat 2.5] }
      let vm' = execute (OPERATOR ADD) vm
      stack vm' `shouldBe` [VFloat 4.0]
      index vm' `shouldBe` 1

    it "executes SUBTRACT for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 5.5, VFloat 2.5] }
      let vm' = execute (OPERATOR SUBTRACT) vm
      stack vm' `shouldBe` [VFloat (-3.0)]
      index vm' `shouldBe` 1

    it "executes MULTIPLY for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 2.0, VFloat 3.0] }
      let vm' = execute (OPERATOR MULTIPLY) vm
      stack vm' `shouldBe` [VFloat 6.0]
      index vm' `shouldBe` 1

    it "executes DIVIDE for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 7.0, VFloat 2.0] }
      let vm' = execute (OPERATOR DIVIDE) vm
      stack vm' `shouldBe` [VFloat 0.2857143]
      index vm' `shouldBe` 1

    it "executes ADD for strings" $ do
      let vm = (initializeVM []) { stack = [VString "world", VString "hello"] }
      let vm' = execute (OPERATOR ADD) vm
      stack vm' `shouldBe` [VString "helloworld"]
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

    it "executes COMPARE_GT for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 5, VInt 3] }
      let vm' = execute (COMPARATOR COMPARE_GT) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_LT for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 3, VInt 5] }
      let vm' = execute (COMPARATOR COMPARE_LT) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_EQ for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 1.5, VFloat 1.5] }
      let vm' = execute (COMPARATOR COMPARE_EQ) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_GT for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 2.5, VFloat 3.5] }
      let vm' = execute (COMPARATOR COMPARE_GT) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_LT for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 3.5, VFloat 2.5] }
      let vm' = execute (COMPARATOR COMPARE_LT) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_EQ for strings" $ do
      let vm = (initializeVM []) { stack = [VString "hello", VString "hello"] }
      let vm' = execute (COMPARATOR COMPARE_EQ) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_GT for strings" $ do
      let vm = (initializeVM []) { stack = [VString "hello", VString "world"] }
      let vm' = execute (COMPARATOR COMPARE_GT) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_LT for strings" $ do
      let vm = (initializeVM []) { stack = [VString "world", VString "hello"] }
      let vm' = execute (COMPARATOR COMPARE_LT) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_NE for strings" $ do
      let vm = (initializeVM []) { stack = [VString "world", VString "hello"] }
      let vm' = execute (COMPARATOR COMPARE_NE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_GE for strings" $ do
      let vm = (initializeVM []) { stack = [VString "world", VString "hello"] }
      let vm' = execute (COMPARATOR COMPARE_GE) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_LE for strings" $ do
      let vm = (initializeVM []) { stack = [VString "world", VString "hello"] }
      let vm' = execute (COMPARATOR COMPARE_LE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_EQ for booleans" $ do
      let vm = (initializeVM []) { stack = [VBool True, VBool False] }
      let vm' = execute (COMPARATOR COMPARE_EQ) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_NE for booleans" $ do
      let vm = (initializeVM []) { stack = [VBool True, VBool False] }
      let vm' = execute (COMPARATOR COMPARE_NE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "throws an error for stack underflow in COMPARATOR" $ do
      let vm = initializeVM []
      evaluate (execute (COMPARATOR COMPARE_EQ) vm) `shouldThrow` anyErrorCall

    it "executes COMPARE_EQ for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 42, VInt 42] }
      let vm' = execute (COMPARATOR COMPARE_EQ) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_EQ for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 1.5, VFloat 1.5] }
      let vm' = execute (COMPARATOR COMPARE_EQ) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_EQ for chars" $ do
      let vm = (initializeVM []) { stack = [VChar 'a', VChar 'a'] }
      let vm' = execute (COMPARATOR COMPARE_EQ) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    -- Tests pour COMPARE_NE
    it "executes COMPARE_NE for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 42, VInt 41] }
      let vm' = execute (COMPARATOR COMPARE_NE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_NE for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 1.5, VFloat 2.5] }
      let vm' = execute (COMPARATOR COMPARE_NE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_NE for chars" $ do
      let vm = (initializeVM []) { stack = [VChar 'a', VChar 'b'] }
      let vm' = execute (COMPARATOR COMPARE_NE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    -- Tests pour COMPARE_GT
    it "executes COMPARE_GT for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 42, VInt 41] }
      let vm' = execute (COMPARATOR COMPARE_GT) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_GT for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 2.5, VFloat 1.5] }
      let vm' = execute (COMPARATOR COMPARE_GT) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_GT for chars" $ do
      let vm = (initializeVM []) { stack = [VChar 'b', VChar 'a'] }
      let vm' = execute (COMPARATOR COMPARE_GT) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    -- Tests pour COMPARE_LT
    it "executes COMPARE_LT for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 41, VInt 42] }
      let vm' = execute (COMPARATOR COMPARE_LT) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_LT for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 1.5, VFloat 2.5] }
      let vm' = execute (COMPARATOR COMPARE_LT) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_LT for chars" $ do
      let vm = (initializeVM []) { stack = [VChar 'a', VChar 'b'] }
      let vm' = execute (COMPARATOR COMPARE_LT) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    -- Tests pour COMPARE_GE
    it "executes COMPARE_GE for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 42, VInt 42] }
      let vm' = execute (COMPARATOR COMPARE_GE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_GE for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 2.5, VFloat 2.5] }
      let vm' = execute (COMPARATOR COMPARE_GE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    it "executes COMPARE_GE for chars" $ do
      let vm = (initializeVM []) { stack = [VChar 'b', VChar 'b'] }
      let vm' = execute (COMPARATOR COMPARE_GE) vm
      stack vm' `shouldBe` [VBool True]
      index vm' `shouldBe` 1

    -- Tests pour COMPARE_LE
    it "executes COMPARE_LE for integers" $ do
      let vm = (initializeVM []) { stack = [VInt 41, VInt 42] }
      let vm' = execute (COMPARATOR COMPARE_LE) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_LE for floats" $ do
      let vm = (initializeVM []) { stack = [VFloat 1.5, VFloat 2.5] }
      let vm' = execute (COMPARATOR COMPARE_LE) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    it "executes COMPARE_LE for chars" $ do
      let vm = (initializeVM []) { stack = [VChar 'a', VChar 'b'] }
      let vm' = execute (COMPARATOR COMPARE_LE) vm
      stack vm' `shouldBe` [VBool False]
      index vm' `shouldBe` 1

    -- Erreurs
    it "throws an error for stack underflow in COMPARATOR" $ do
      let vm = initializeVM []
      evaluate (execute (COMPARATOR COMPARE_EQ) vm) `shouldThrow` anyErrorCall

  describe "LABEL_FUNC and LABEL_FUNC_END" $ do
    it "executes LABEL_FUNC when the call stack is empty and finds the end label" $ do
      let vm = (initializeVM [LABEL_FUNC "myFunc", LABEL_FUNC_END "myFunc"]) { index = 0 }
      let vm' = execute (LABEL_FUNC "myFunc") vm
      index vm' `shouldBe` 2

    it "executes LABEL_FUNC when the call stack is not empty" $ do
      let vm = (initializeVM []) { callStack = [10], index = 5 }
      let vm' = execute (LABEL_FUNC "myFunc") vm
      index vm' `shouldBe` 6

    it "throws an error when LABEL_FUNC_END is not found" $ do
      let vm = initializeVM [LABEL_FUNC "myFunc"]
      evaluate (execute (LABEL_FUNC "myFunc") vm) `shouldThrow` anyErrorCall

    it "executes LABEL_FUNC_END and pops the call stack" $ do
      let vm = (initializeVM []) { callStack = [5], index = 10 }
      let vm' = execute (LABEL_FUNC_END "myFunc") vm
      index vm' `shouldBe` 5
      callStack vm' `shouldBe` []

    it "executes LABEL_FUNC_END when the call stack is empty" $ do
      let vm = (initializeVM []) { index = 5 }
      let vm' = execute (LABEL_FUNC_END "myFunc") vm
      index vm' `shouldBe` 6

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
    it "executes JUMP_IF_FALSE when the top of the stack is False" $ do
      let vm = (initializeVM [LABEL "target"]) { stack = [VBool False], index = 1 }
      let vm' = execute (JUMP_IF_FALSE "target") vm
      index vm' `shouldBe` 0
      stack vm' `shouldBe` []

    it "does not jump when the top of the stack is True" $ do
      let vm = (initializeVM [LABEL "target"]) { stack = [VBool True], index = 1 }
      let vm' = execute (JUMP_IF_FALSE "target") vm
      index vm' `shouldBe` 2
      stack vm' `shouldBe` []

    it "throws an error for invalid stack type" $ do
      let vm = (initializeVM []) { stack = [VInt 42] }
      evaluate (execute (JUMP_IF_FALSE "target") vm) `shouldThrow` anyErrorCall

    it "throws an error for stack underflow" $ do
      let vm = initializeVM []
      evaluate (execute (JUMP_IF_FALSE "target") vm) `shouldThrow` anyErrorCall

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

  describe "LABEL" $ do
    it "executes LABEL and increments the index" $ do
      let vm = (initializeVM [LABEL "start"]) { index = 0 }
      let vm' = execute (LABEL "start") vm
      index vm' `shouldBe` 1

  describe "RETURN" $ do
    it "executes RETURN and pops the call stack" $ do
      let vm = (initializeVM []) { callStack = [10], index = 5 }
      let vm' = execute RETURN vm
      index vm' `shouldBe` 10
      callStack vm' `shouldBe` []

    it "throws an error when RETURN is called outside a function" $ do
      let vm = (initializeVM []) { callStack = [] }
      evaluate (execute RETURN vm) `shouldThrow` anyErrorCall

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
