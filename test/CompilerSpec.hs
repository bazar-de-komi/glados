module CompilerSpec (spec) where

import Test.Hspec
import Compiler.GenerateBytecode (generateInstructionsList)
import Structure (AST(..), Instruction(..), Value(..), BinaryOperator(..), BinaryComparator(..))

buildAssignment :: String -> AST -> AST
buildAssignment var value = SList [SVariable var, SOperation "=", value]

buildBinaryOperation :: String -> AST -> AST -> AST
buildBinaryOperation op left right = SList [left, SOperation op, right]

buildFunctionCall :: String -> [AST] -> AST
buildFunctionCall name args = SCall name (SList args)

spec:: Spec
spec = do
    describe "Bytecode Generator" $ do
        testAssigment
        testInstruction
        testOperators
        testCondition
        testFunctionCall

testAssigment :: Spec
testAssigment = describe "Assigment" $ do
    it "handle with list assignment" $ do
            let ast = SList [SVariable "x", SOperation "=", SList [SInt 5]]
            generateInstructionsList ast `shouldBe` 
                [STORE_CONST (VInt 5), STORE_VAR "x"]
    
    it "handle simple assignation" $ do
      let ast = SList [SVariable "x", SOperation "=", SInt 42]
      let expected = [STORE_CONST (VInt 42), STORE_VAR "x"]
      generateInstructionsList ast `shouldBe` expected

testInstruction :: Spec
testInstruction = describe "Instruction" $ do
    it "handle STORE_CONST instruction" $ do
      let ast = SInt 42
      let expected = [STORE_CONST (VInt 42)]
      generateInstructionsList ast `shouldBe` expected

    it "handle STORE_VAR instruction" $ do
      let ast = buildAssignment "x" (SInt 42)
      let expected = [STORE_CONST (VInt 42), STORE_VAR "x"]
      generateInstructionsList ast `shouldBe` expected

    it "handle binary operation" $ do
      let ast = buildBinaryOperation "+" (SInt 10) (SInt 32)
      let expected = [STORE_CONST (VInt 10), STORE_CONST (VInt 32), OPERATOR ADD]
      generateInstructionsList ast `shouldBe` expected

    it "handle empty AST" $ do
      let ast = SList []
      let expected = []
      generateInstructionsList ast `shouldBe` expected

    it "handle an undefined variable" $ do
      let ast = SVariable "undefinedVar"
      let expected = [LOAD_VAR "undefinedVar"]
      generateInstructionsList ast `shouldBe` expected

testOperators :: Spec
testOperators = describe "Operators" $ do
    it "handle addition" $ do
      let ast = SList [SInt 3, SOperation "+", SInt 4]
      generateInstructionsList ast `shouldBe` 
        [STORE_CONST (VInt 3), STORE_CONST (VInt 4), OPERATOR ADD]
    
    it "handle subtraction" $ do
      let ast = SList [SInt 10, SOperation "-", SInt 6]
      generateInstructionsList ast `shouldBe` 
        [STORE_CONST (VInt 10), STORE_CONST (VInt 6), OPERATOR SUBTRACT]
    
    it "handle multiplication" $ do
      let ast = SList [SInt 4, SOperation "*", SInt 2]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 4), STORE_CONST (VInt 2), OPERATOR MULTIPLY]

    it "handle division" $ do
      let ast = SList [SInt 8, SOperation "/", SInt 4]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 8), STORE_CONST (VInt 4), OPERATOR DIVIDE]

    it "handle modulo" $ do
      let ast = SList [SInt 10, SOperation "%", SInt 3]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 10), STORE_CONST (VInt 3), OPERATOR MODULO]

    it "handle greater than comparison" $ do
      let ast = SList [SInt 5, SOperation ">", SInt 3]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 5), STORE_CONST (VInt 3), COMPARATOR COMPARE_GT]

    it "handle less than comparison" $ do
      let ast = SList [SInt 2, SOperation "<", SInt 5]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 2), STORE_CONST (VInt 5), COMPARATOR COMPARE_LT]

    it "handle equality comparison" $ do
      let ast = SList [SInt 3, SOperation "==", SInt 3]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 3), STORE_CONST (VInt 3), COMPARATOR COMPARE_EQ]

    it "handle inequality comparison" $ do
      let ast = SList [SInt 3, SOperation "!=", SInt 4]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 3), STORE_CONST (VInt 4), COMPARATOR COMPARE_NE]
    
    it "handle greater than or equal comparison" $ do
      let ast = SList [SInt 5, SOperation ">=", SInt 5]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 5), STORE_CONST (VInt 5), COMPARATOR COMPARE_GE]

    it "handle less than or equal comparison" $ do
      let ast = SList [SInt 3, SOperation "<=", SInt 5]
      generateInstructionsList ast `shouldBe`
        [STORE_CONST (VInt 3), STORE_CONST (VInt 5), COMPARATOR COMPARE_LE]

testCondition :: Spec
testCondition = describe "Condition" $ do
    it "handle if statement" $ do
      let ast = SIf 
                  (SList [SVariable "x", SOperation ">", SInt 5]) 
                  (SList [SInt 10]) 
                  (SList [SInt 20])
      generateInstructionsList ast `shouldBe` 
        [ LOAD_VAR "x"
        , STORE_CONST (VInt 5)
        , COMPARATOR COMPARE_GT
        , JUMP_IF_FALSE "if_0"
        , STORE_CONST (VInt 10)
        , JUMP "if_1"
        , LABEL "if_0"
        , STORE_CONST (VInt 20)
        , LABEL "if_1"
        ]

testFunctionCall :: Spec
testFunctionCall = describe "Function call" $ do
    it "handle function call" $ do
      let ast = SCall "add" (SList [SInt 5, SInt 3])
      generateInstructionsList ast `shouldBe`
        [ STORE_CONST (VInt 3)
        , STORE_CONST (VInt 5)
        , CALL "add"
        ]

    it "handle my function call" $ do
      let ast = buildFunctionCall "myFunction" [SInt 10, SInt 20]
      let expected = [STORE_CONST (VInt 20), STORE_CONST (VInt 10), CALL "myFunction"]
      generateInstructionsList ast `shouldBe` expected

