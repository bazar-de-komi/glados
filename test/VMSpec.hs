module VMSpec (spec) where

import Test.Hspec
import Execute.Execute (execute)

spec :: Spec
spec = do
    describe "HandleFunctions" $ do
        testReturnValueCheck

testReturnValueCheck :: Spec
testReturnValueCheck =
    describe "returnValueCheck" $ do
        testStoreConst
        testStoreVar
        testLoadVar
        testOperator
        testComparator
        testJump
        testHalt

testStoreConst :: Spec
testStoreConst =
    describe "storeConst" $ do
        it "Pushes a value on the stack" $ do
            let vm = initialState
            let result = handleStoreConst vm (IntVal 42)
            result `shouldBe` Right vm { stack = [IntVal 42] }

testStoreVar :: Spec
testStoreVar =
    describe "storeVar" $ do
        it "Stores a variable from the stack" $ do
            let vm = initialState { stack = [IntVal 42] }
            let result = handleStoreVar vm "x"
            result `shouldBe` Right vm { stack = [], variables = Map.singleton "x" (IntVal 42) }

        it "Fails when the stack is empty" $ do
            let vm = initialState
            let result = handleStoreVar vm "x"
            result `shouldBe` Left "Error: Stack underflow on STORE_VAR."

testLoadVar :: Spec
testLoadVar =
    describe "loadVar" $ do
        it "Loads a variable onto the stack" $ do
            let vm = initialState { variables = Map.singleton "x" (IntVal 42) }
            let result = handleLoadVar vm "x"
            result `shouldBe` Right vm { stack = [IntVal 42] }

        it "Fails when the variable is not found" $ do
            let vm = initialState
            let result = handleLoadVar vm "x"
            result `shouldBe` Left "Error: Variable not found: x."

testOperator :: Spec
testOperator =
    describe "operator" $ do
        it "Performs addition" $ do
            let vm = initialState { stack = [IntVal 3, IntVal 4] }
            let result = handleOperator vm ADD
            result `shouldBe` Right vm { stack = [IntVal 7] }

        it "Fails on division by zero" $ do
            let vm = initialState { stack = [IntVal 0, IntVal 4] }
            let result = handleOperator vm DIVIDE
            result `shouldBe` Left "Error: Division by zero."

        it "Fails on stack underflow" $ do
            let vm = initialState { stack = [IntVal 4] }
            let result = handleOperator vm ADD
            result `shouldBe` Left "Error: Operator requires two values on the stack."

testComparator :: Spec
testComparator =
    describe "comparator" $ do
        it "Performs greater-than comparison" $ do
            let vm = initialState { stack = [IntVal 3, IntVal 4] }
            let result = handleComparator vm COMPARE_GT
            result `shouldBe` Right vm { stack = [BoolVal False] }

        it "Fails on stack underflow" $ do
            let vm = initialState { stack = [IntVal 4] }
            let result = handleComparator vm COMPARE_GT
            result `shouldBe` Left "Error: Comparator requires two integers on the stack."

testJump :: Spec
testJump =
    describe "jump" $ do
        it "Jumps to a valid label" $ do
            let vm = initialState { labels = Map.singleton "start" 0 }
            let result = handleJump vm "start"
            result `shouldBe` Right vm { index = 0 }

        it "Fails for an undefined label" $ do
            let vm = initialState
            let result = handleJump vm "start"
            result `shouldBe` Left "Error: Label not found: start."

testHalt :: Spec
testHalt =
    describe "halt" $ do
        it "Stops the program" $ do
            let vm = initialState { program = ["HALT"], index = 0 }
            let result = handleHalt vm
            result `shouldBe` Right vm { program = [], index = -1 }
