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
        testConditionalExpressions
        testForLoop
        testFunction
        testSDefine
        testStoreConstants

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

testConditionalExpressions :: Spec
testConditionalExpressions = describe "Conditional Expressions with Calculation" $ do
    it "handle addition followed by comparison" $ do
        let ast = SList [SInt 3, SOperation "+", SInt 4, SOperation ">", SInt 6]
        let expected = 
                [ STORE_CONST (VInt 3)
                , STORE_CONST (VInt 4)
                , OPERATOR ADD
                , STORE_CONST (VInt 6)
                , COMPARATOR COMPARE_GT
                ]
        generateInstructionsList ast `shouldBe` expected

    it "handle multiplication followed by equality comparison" $ do
        let ast = SList [SInt 2, SOperation "*", SInt 3, SOperation "==", SInt 6]
        let expected = 
                [ STORE_CONST (VInt 2)
                , STORE_CONST (VInt 3)
                , OPERATOR MULTIPLY
                , STORE_CONST (VInt 6)
                , COMPARATOR COMPARE_EQ
                ]
        generateInstructionsList ast `shouldBe` expected

    it "handle comparison followed by subtraction" $ do
        let ast = SList [SInt 10, SOperation ">", SInt 5, SOperation "-", SInt 3]
        let expected = 
                [ STORE_CONST (VInt 5)
                , STORE_CONST (VInt 3)
                , OPERATOR SUBTRACT
                , STORE_CONST (VInt 10)
                , COMPARATOR COMPARE_GT
                ]
        generateInstructionsList ast `shouldBe` expected

    it "handle less than or equal comparison followed by multiplication" $ do
      let ast = SList [SInt 4, SOperation "<=", SInt 6, SOperation "*", SInt 3]
      let expected = 
              [ STORE_CONST (VInt 6)
              , STORE_CONST (VInt 3)
              , OPERATOR MULTIPLY
              , STORE_CONST (VInt 4)
              , COMPARATOR COMPARE_LE
              ]
      generateInstructionsList ast `shouldBe` expected

    it "handle modulo followed by less than comparison" $ do
        let ast = SList [SInt 10, SOperation "%", SInt 3, SOperation "<", SInt 2]
        let expected = 
                [ STORE_CONST (VInt 10)
                , STORE_CONST (VInt 3)
                , OPERATOR MODULO
                , STORE_CONST (VInt 2)
                , COMPARATOR COMPARE_LT
                ]
        generateInstructionsList ast `shouldBe` expected

    it "handle greater than or equal followed by division" $ do
        let ast = SList [SInt 8, SOperation ">=", SInt 4, SOperation "/", SInt 2]
        let expected = 
                [ STORE_CONST (VInt 4)
                , STORE_CONST (VInt 2)
                , OPERATOR DIVIDE
                , STORE_CONST (VInt 8)
                , COMPARATOR COMPARE_GE
                ]
        generateInstructionsList ast `shouldBe` expected

testForLoop :: Spec
testForLoop = describe "For Loop" $ do
      it "handles a simple for loop" $ do
        -- AST : for (x = 0; x < 5; x = x + 1) { y = y + 2 }
        let ast = SFor 
                    (SList [SVariable "x", SOperation "=", SInt 0]) -- Init
                    (SList [SVariable "x", SOperation "<", SInt 5])  -- Condition
                    (SList [SVariable "x", SOperation "=", SList [SVariable "x", SOperation "+", SInt 1]]) -- update
                    (SList [SVariable "y", SOperation "=", SList [SVariable "y", SOperation "+", SInt 2]]) -- body
        let expected = 
                [ STORE_CONST (VInt 0)          -- Init : x = 0
                , STORE_VAR "x"
                , LABEL "loop_0"               -- label for start
                , LOAD_VAR "x"                 -- Condition : x < 5
                , STORE_CONST (VInt 5)
                , COMPARATOR COMPARE_LT
                , JUMP_IF_FALSE "loop_1"       -- jump if condition false
                , LOAD_VAR "y"                 -- body : y = y + 2
                , STORE_CONST (VInt 2)
                , OPERATOR ADD
                , STORE_VAR "y"
                , LOAD_VAR "x"                 -- update : x = x + 1
                , STORE_CONST (VInt 1)
                , OPERATOR ADD
                , STORE_VAR "x"
                , JUMP "loop_0"                -- back start of the loop
                , LABEL "loop_1"               -- label end loop
                ]
        generateInstructionsList ast `shouldBe` expected

testFunction :: Spec
testFunction = describe "Function" $ do
  it "handle a function with no parameters" $ do
    -- AST : function myFunc() { return 42; }
    let ast = SFunc "myFunc" (SType "Void") (SList []) 
                (SList [SReturn (SInt 42)])
    let expected = 
            [ LABEL_FUNC "myFunc"
            , STORE_CONST (VInt 42)          -- return 42
            , RETURN
            , LABEL_FUNC_END "myFunc"
            ]
    generateInstructionsList ast `shouldBe` expected

  it "handle a function with simple parameters" $ do
  -- AST : function myFunc(x, y) { return x + y; }
    let ast = SFunc "myFunc" (SType "Int") 
                (SList [SType "Int", SList [SVariable "x", SVariable "y"]]) 
                (SList [SReturn (SList [SVariable "x", SOperation "+", SVariable "y"])])
    let expected = 
            [ LABEL_FUNC "myFunc"
            , STORE_VAR "x"                 
            , STORE_VAR "y"                  
            , LOAD_VAR "x"                   -- body : x + y
            , LOAD_VAR "y"
            , OPERATOR ADD
            , RETURN
            , LABEL_FUNC_END "myFunc"
            ]
    generateInstructionsList ast `shouldBe` expected

  it "handle a function with an empty body" $ do
  -- AST : function myFunc() {}
    let ast = SFunc "myFunc" (SType "Void") (SList []) (SList [])
    let expected =
            [ LABEL_FUNC "myFunc"
            , LABEL_FUNC_END "myFunc"
            ]
    generateInstructionsList ast `shouldBe` expected

testSDefine :: Spec
testSDefine = describe "Variable Definition (SDefine)" $ do
  it "handles defining a variable with an integer value" $ do
    -- AST : int x = 42;
    let ast = SDefine "x" (SType "Int") (SInt 42)
    let expected =
            [ STORE_CONST (VInt 42)  
            , STORE_VAR "x"          
            ]
    generateInstructionsList ast `shouldBe` expected

  it "handles defining a variable with an arithmetic expression" $ do
    -- AST : int y = 10 + 32;
    let ast = SDefine "y" (SType "Int") 
                (SList [SInt 10, SOperation "+", SInt 32])
    let expected =
            [ STORE_CONST (VInt 10)  
            , STORE_CONST (VInt 32)  
            , OPERATOR ADD           
            , STORE_VAR "y"          
            ]
    generateInstructionsList ast `shouldBe` expected

  it "handles defining a variable with another variable as its value" $ do
    -- AST : int z = y;
    let ast = SDefine "z" (SType "Int") (SVariable "y")
    let expected =
            [ LOAD_VAR "y"          
            , STORE_VAR "z"          
            ]
    generateInstructionsList ast `shouldBe` expected

  it "handles defining a variable with a nested arithmetic expression" $ do
    -- AST : int w = (x * 2) + 3;
    let ast = SDefine "w" (SType "Int") 
                (SList 
                  [ SList [SVariable "x", SOperation "*", SInt 2]
                  , SOperation "+"
                  , SInt 3
                  ])
    let expected =
            [ LOAD_VAR "x"          
            , STORE_CONST (VInt 2)   
            , OPERATOR MULTIPLY      
            , STORE_CONST (VInt 3)   
            , OPERATOR ADD          
            , STORE_VAR "w"          
            ]
    generateInstructionsList ast `shouldBe` expected

  it "handles defining a variable with a custom type" $ do
    -- AST : customType obj = 5;
    let ast = SDefine "obj" (SType "CustomType") (SInt 5)
    let expected =
            [ STORE_CONST (VInt 5)  
            , STORE_VAR "obj"       
            ]
    generateInstructionsList ast `shouldBe` expected

testStoreConstants :: Spec
testStoreConstants = describe "Store Constants" $ do

  it "handles storing an integer constant" $ do
    -- AST : 42
    let ast = SInt 42
    let expected =
            [ STORE_CONST (VInt 42) ]
    generateInstructionsList ast `shouldBe` expected

  it "handles storing a float constant" $ do
    -- AST : 3.14
    let ast = SFloat 3.14
    let expected =
            [ STORE_CONST (VFloat 3.14) ]  
    generateInstructionsList ast `shouldBe` expected

  it "handles storing a boolean constant (true)" $ do
    -- AST : true
    let ast = SBool True
    let expected =
            [ STORE_CONST (VBool True) ]  
    generateInstructionsList ast `shouldBe` expected

  it "handles storing a boolean constant (false)" $ do
    -- AST : false
    let ast = SBool False
    let expected =
            [ STORE_CONST (VBool False) ]  
    generateInstructionsList ast `shouldBe` expected

  it "handles storing a string constant" $ do
    -- AST : "hello"
    let ast = SString "hello"
    let expected =
            [ STORE_CONST (VString "hello") ]  
    generateInstructionsList ast `shouldBe` expected

  it "handles storing a character constant" $ do
    -- AST : 'a'
    let ast = SChar 'a'
    let expected =
            [ STORE_CONST (VChar 'a') ]  
    generateInstructionsList ast `shouldBe` expected
