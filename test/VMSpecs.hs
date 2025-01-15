module VMSpecs (spec) where

import Test.Hspec
import RunVM
import Structure
import qualified Data.Map as Map
import Control.Exception (evaluate)

spec :: Spec
spec = do
    describe "HandleFunctions" $ do
        testReturnValueCheck

testReturnValueCheck :: Spec
testReturnValueCheck =
    describe "returnValueCheck" $ do
        let vmSample = initializeVM [STORE_CONST (VInt 5), STORE_CONST (VInt 10), OPERATOR ADD]

        it "should initialize a VM with an empty stack and empty variables" $ do
            let vm = initializeVM []
            stack vm `shouldBe` []
            variables vm `shouldBe` Map.empty
            index vm `shouldBe` 0

        it "should execute STORE_CONST correctly" $ do
            let vm = execute (STORE_CONST (VInt 5)) vmSample
            stack vm `shouldBe` [VInt 5]
            index vm `shouldBe` 1

        it "should execute STORE_VAR correctly" $ do
            let vm = execute (STORE_VAR "x") vmSample
            let vmAfter = execute (STORE_VAR "y") vm
            variables vmAfter `shouldBe` Map.fromList [("x", VInt 5), ("y", VInt 10)]
            stack vmAfter `shouldBe` []
            index vmAfter `shouldBe` 2

        it "should execute OPERATOR ADD correctly" $ do
            let vmAfterOp = execute (OPERATOR ADD) vmSample
            stack vmAfterOp `shouldBe` [VInt 15]  -- 5 + 10 = 15
            index vmAfterOp `shouldBe` 3

        it "should handle stack underflow in STORE_VAR" $ do
            let vmUnderflow = initializeVM [STORE_VAR "x"]
            evaluate (execute (STORE_VAR "x") vmUnderflow) `shouldThrow` anyException

        it "should execute a function call correctly" $ do
            let instrs = [LABEL "func", STORE_CONST (VInt 42), RETURN]
            let vm = initializeVM instrs
            let vmAfterCall = execute (CALL "func") vm
            index vmAfterCall `shouldBe` 1  -- It should jump to index 1 (LABEL "func")

        it "should handle label jumps correctly" $ do
            let instrs = [LABEL "start", JUMP "end", LABEL "end"]
            let vm = initializeVM instrs
            let vmAfterJump = execute (JUMP "end") vm
            index vmAfterJump `shouldBe` 2

        it "should execute JUMP_IF_FALSE correctly" $ do
            let instrs = [STORE_CONST (VBool False), JUMP_IF_FALSE "end", LABEL "end"]
            let vm = initializeVM instrs
            let vmAfterJump = execute (JUMP_IF_FALSE "end") vm
            index vmAfterJump `shouldBe` 2  -- Should jump to "end" because the value is False

        it "should halt execution correctly" $ do
            let vmAfterHalt = execute HALT vmSample
            index vmAfterHalt `shouldBe` (-1)  -- The index should be set to -1 after HALT

        it "should correctly handle function labels" $ do
            let instrs = [LABEL_FUNC "foo", STORE_CONST (VInt 100), LABEL_FUNC_END "foo"]
            let vm = initializeVM instrs
            let vmAfter = execute (LABEL_FUNC "foo") vm
            index vmAfter `shouldBe` 1  -- Jump to label function start
            let vmEnd = execute (LABEL_FUNC_END "foo") vmAfter
            index vmEnd `shouldBe` 0  -- Return to the previous instruction after the function ends
