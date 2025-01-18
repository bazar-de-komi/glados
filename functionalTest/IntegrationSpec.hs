module IntegrationSpec (spec) where

import Test.Hspec
import GenerateBytecode (generateInstructionsList)
import Structure (AST(..), Instruction(..), Value(..), BinaryOperator(..), BinaryComparator(..))

spec :: Spec
spec = do
  describe "Functional Bytecode Tests" $ do
    testSimpleProgram
    testFullProgram

testSimpleProgram :: Spec
testSimpleProgram = it "handles a simple program with assignment and condition" $ do
  let program = SList
        [ SDefine "x" (SType "int") (SInt 10)
        , SIf
            (SList [SVariable "x", SOperation ">", SInt 5])
            (SList [SVariable "x", SOperation "=", SInt 20])
            (SList [SVariable "x", SOperation "=", SInt 0])
        ]
  let expected = 
        [ STORE_CONST (VInt 10)
        , STORE_VAR "x"
        , LOAD_VAR "x"
        , STORE_CONST (VInt 5)
        , COMPARATOR COMPARE_GT
        , JUMP_IF_FALSE "if_0"
        , STORE_CONST (VInt 20)
        , STORE_VAR "x"
        , JUMP "if_1"
        , LABEL "if_0"
        , STORE_CONST (VInt 0)
        , STORE_VAR "x"
        , LABEL "if_1"
        ]
  generateInstructionsList program `shouldBe` expected

testFullProgram :: Spec
testFullProgram = it "handles a full program with function definition and call" $ do
  let program = SList
        [ SFunc
            "add"
            (SType "int")
            (SList [SType "int", SList [SVariable "a", SVariable "b"]])
            (SList [SReturn (SList [SVariable "a", SOperation "+", SVariable "b"])])
        , SCall "add" (SList [SInt 5, SInt 3])
        ]
  let expected = 
        [ LABEL_FUNC "add"
        , STORE_VAR "a"
        , STORE_VAR "b"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , OPERATOR ADD
        , RETURN
        , LABEL_FUNC_END "add"
        , STORE_CONST (VInt 3)
        , STORE_CONST (VInt 5)
        , CALL "add"
        ]
  generateInstructionsList program `shouldBe` expected

