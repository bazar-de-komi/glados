module CompilerSpec (spec) where

import Test.Hspec
import Compiler.GenerateBytecode (generateInstructionsList)
import Structure (AST(..), Instruction(..), Value(..), BinaryOperator(..), BinaryComparator(..))

spec :: Spec
spec = do
  describe "Functional Bytecode Tests" $ do
    testSimpleProgram
    testFullProgram
    testOperators
    testConditionalCalculation
    testNestedOperations
    testOperatorPrecedence
    testArithmeticOperators
    testModuloOperator
    testForLoopSimple
    testForLoopNested
    testForLoopConditionComplex

testSimpleProgram :: Spec
testSimpleProgram = it "handle a simple program with assignment and condition" $ do
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
testFullProgram = it "handle a full program with function definition and call" $ do
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

testOperators :: Spec
testOperators = it "handle various binary operators and comparators" $ do
  let program = SList
        [ SList [SDefine "a" (SType "int") (SInt 10)]
        , SList [SDefine "b" (SType "int") (SInt 5)]
        , SIf
            (SList [SVariable "a", SOperation "-", SVariable "b"])
            (SList [SVariable "a", SOperation "*", SVariable "b"])
            (SList [SVariable "a", SOperation "/", SVariable "b"])
        , SList [SVariable "a", SOperation "%", SVariable "b"]
        , SIf
            (SList [SVariable "a", SOperation "<", SVariable "b"])
            (SList [SVariable "a", SOperation "==", SVariable "b"])
            (SList [SVariable "a", SOperation "!=", SVariable "b"])
        , SIf
            (SList [SVariable "a", SOperation ">=", SVariable "b"])
            (SList [SVariable "a", SOperation "<=", SVariable "b"])
            (SList [])
        ]

  let expected = 
        [ STORE_CONST (VInt 10)
        , STORE_VAR "a"
        , STORE_CONST (VInt 5)
        , STORE_VAR "b"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , OPERATOR SUBTRACT
        , JUMP_IF_FALSE "if_0"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , OPERATOR MULTIPLY
        , JUMP "if_1"
        , LABEL "if_0"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , OPERATOR DIVIDE
        , LABEL "if_1"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , OPERATOR MODULO
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , COMPARATOR COMPARE_LT
        , JUMP_IF_FALSE "if_2"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , COMPARATOR COMPARE_EQ
        , JUMP "if_3"
        , LABEL "if_2"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , COMPARATOR COMPARE_NE
        , LABEL "if_3"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , COMPARATOR COMPARE_GE
        , JUMP_IF_FALSE "if_4"
        , LOAD_VAR "a"
        , LOAD_VAR "b"
        , COMPARATOR COMPARE_LE
        , JUMP "if_5"
        , LABEL "if_4"
        , LABEL "if_5"
        ]
  generateInstructionsList program `shouldBe` expected

testConditionalCalculation :: Spec
testConditionalCalculation = it "handle conditional calculations" $ do
  let program = SList 
        [ SVariable "x"
        , SOperation "+"
        , SInt 10
        , SOperation "<"
        , SInt 20
        ]
  let expected =
        [ LOAD_VAR "x"
        , STORE_CONST (VInt 10)
        , OPERATOR ADD
        , STORE_CONST (VInt 20)
        , COMPARATOR COMPARE_LT
        ]
  generateInstructionsList program `shouldBe` expected

testNestedOperations :: Spec
testNestedOperations = it "handle nested operations" $ do
  let program = SList 
        [ SVariable "a"
        , SOperation "*"
        , SVariable "b"
        , SOperation ">"
        , SList [SInt 10, SOperation "+", SInt 5]
        ]
  let expected =
        [ LOAD_VAR "a"
        , LOAD_VAR "b"
        , OPERATOR MULTIPLY
        , STORE_CONST (VInt 10)
        , STORE_CONST (VInt 5)
        , OPERATOR ADD
        , COMPARATOR COMPARE_GT
        ]
  generateInstructionsList program `shouldBe` expected

testOperatorPrecedence :: Spec
testOperatorPrecedence = it "respects operator precedence" $ do
  let program = SList 
        [ SVariable "x"
        , SOperation "+"
        , SVariable "y"
        , SOperation "*"
        , SVariable "z"
        ]
  let expected =
        [ LOAD_VAR "x"
        , LOAD_VAR "y"
        , OPERATOR ADD
        , LOAD_VAR "z"
        , OPERATOR MULTIPLY
        ]
  generateInstructionsList program `shouldBe` expected

testArithmeticOperators :: Spec
testArithmeticOperators = it "handles arithmetic operators - and /" $ do
  let program = SList 
        [ SVariable "x"
        , SOperation "-"
        , SInt 5
        , SOperation "/"
        , SVariable "y"
        ]
  let expected =
        [ LOAD_VAR "x"
        , STORE_CONST (VInt 5)
        , OPERATOR SUBTRACT
        , LOAD_VAR "y"
        , OPERATOR DIVIDE
        ]
  generateInstructionsList program `shouldBe` expected

testModuloOperator :: Spec
testModuloOperator = it "handles modulo operator %" $ do
  let program = SList 
        [ SVariable "a"
        , SOperation "%"
        , SVariable "b"
        ]
  let expected =
        [ LOAD_VAR "a"
        , LOAD_VAR "b"
        , OPERATOR MODULO
        ]
  generateInstructionsList program `shouldBe` expected

testForLoopSimple :: Spec
testForLoopSimple = it "handles a simple for loop" $ do
  let program = SFor
        (SDefine "i" (SType "int") (SInt 0)) 
        (SList [SVariable "i", SOperation "<", SInt 1])
        (SList [SVariable "i", SOperation "=", SList [SVariable "i", SOperation "+", SInt 1]]) -- Mise à jour
        (SList [SVariable "i"]) 
  let expected =
        [ STORE_CONST (VInt 0)         
        , STORE_VAR "i"
        , LABEL "loop_0"              
        , LOAD_VAR "i"               
        , STORE_CONST (VInt 1)
        , COMPARATOR COMPARE_LT
        , JUMP_IF_FALSE "loop_1"      
        , LOAD_VAR "i"                
        , LOAD_VAR "i"               
        , STORE_CONST (VInt 1)
        , OPERATOR ADD
        , STORE_VAR "i"
        , JUMP "loop_0"              
        , LABEL "loop_1"            
        ]
  generateInstructionsList program `shouldBe` expected

testForLoopNested :: Spec
testForLoopNested = it "handles nested for loops" $ do
  let program = SFor
        (SDefine "i" (SType "int") (SInt 0)) 
        (SList [SVariable "i", SOperation "<", SInt 3])
        (SList [SVariable "i", SOperation "=", SList [SVariable "i", SOperation "+", SInt 1]])
        (SFor
          (SDefine "j" (SType "int") (SInt 0)) 
          (SList [SVariable "j", SOperation "<", SInt 2])
          (SList [SVariable "j", SOperation "=", SList [SVariable "j", SOperation "+", SInt 1]])
          (SList [SVariable "i", SVariable "j"])
        )
  let expected =
        [ STORE_CONST (VInt 0)          
        , STORE_VAR "i"
        , LABEL "loop_0"               
        , LOAD_VAR "i"               
        , STORE_CONST (VInt 3)
        , COMPARATOR COMPARE_LT
        , JUMP_IF_FALSE "loop_1"      
        , STORE_CONST (VInt 0)         
        , STORE_VAR "j"
        , LABEL "loop_2"             
        , LOAD_VAR "j"               
        , STORE_CONST (VInt 2)
        , COMPARATOR COMPARE_LT
        , JUMP_IF_FALSE "loop_3"     
        , LOAD_VAR "i"               
        , LOAD_VAR "j"
        , LOAD_VAR "j"                
        , STORE_CONST (VInt 1)
        , OPERATOR ADD
        , STORE_VAR "j"
        , JUMP "loop_2"             
        , LABEL "loop_3"            
        , LOAD_VAR "i"               
        , STORE_CONST (VInt 1)
        , OPERATOR ADD
        , STORE_VAR "i"
        , JUMP "loop_0"               
        , LABEL "loop_1"            
        ]
  generateInstructionsList program `shouldBe` expected

testForLoopConditionComplex :: Spec
testForLoopConditionComplex = it "handles a for loop with a complex condition" $ do
  let program = SFor
        (SDefine "x" (SType "int") (SInt 10)) 
        (SList [SVariable "x", SOperation ">", SInt 0]) 
        (SList [SVariable "x", SOperation "=", SList [SVariable "x", SOperation "-", SInt 1]]) -- Mise à jour
        (SList [SVariable "x"]) 
  let expected =
        [ STORE_CONST (VInt 10)        
        , STORE_VAR "x"
        , LABEL "loop_0"             
        , LOAD_VAR "x"               
        , STORE_CONST (VInt 0)
        , COMPARATOR COMPARE_GT
        , JUMP_IF_FALSE "loop_1"      
        , LOAD_VAR "x"              
        , LOAD_VAR "x"                
        , STORE_CONST (VInt 1)
        , OPERATOR SUBTRACT
        , STORE_VAR "x"
        , JUMP "loop_0"               
        , LABEL "loop_1"            
        ]
  generateInstructionsList program `shouldBe` expected
