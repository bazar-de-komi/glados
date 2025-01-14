module GenerateBytecode
  ( generateInstructionsList,
  )
where

import Structure (AST(..), Instruction(..), BinaryOperator (..), BinaryComparator (..), Value(..), LabelState(..))

generateBytecode :: AST -> LabelState -> ([Instruction], LabelState)

-- Assignment
generateBytecode (SList (SVariable var : SOperation "=" : expr)) state =
  let (exprInstructions, newState) = generateBytecode (SList expr) state
  in (exprInstructions ++ [STORE_VAR var], newState)

-- Operations
generateBytecode (SList [leftValue, SOperation op, rightValue]) state =
  let (leftInstructions, state1) = generateBytecode leftValue state
      (rightInstructions, state2) = generateBytecode rightValue state1
      opInstruction = case op of
        "+" -> [OPERATOR ADD]
        "-" -> [OPERATOR SUBTRACT]
        "*" -> [OPERATOR MULTIPLY]
        "/" -> [OPERATOR DIVIDE]
        "%" -> [OPERATOR MODULO]
        ">" -> [COMPARATOR COMPARE_GT]
        "<" -> [COMPARATOR COMPARE_LT]
        "==" -> [COMPARATOR COMPARE_EQ]
        "!=" -> [COMPARATOR COMPARE_NE]
        ">=" -> [COMPARATOR COMPARE_GE]
        "<=" -> [COMPARATOR COMPARE_LE]
        _   -> error ("Unsupported operation: " ++ op)
  in (leftInstructions ++ rightInstructions ++ opInstruction, state2)

-- Case for managing conditional expressions with a calculation
generateBytecode (SList [leftValue, SOperation op1, middleValue, SOperation op2, rightValue]) state =
  let (leftInstructions, state1) = generateBytecode leftValue state
      (middleInstructions, state2) = generateBytecode middleValue state1
      (rightInstructions, state3) = generateBytecode rightValue state2
      opCalculationsMap = 
        [ ("%", OPERATOR MODULO)
        , ("+", OPERATOR ADD)
        , ("*", OPERATOR MULTIPLY)
        , ("-", OPERATOR SUBTRACT)
        , ("/", OPERATOR DIVIDE)
        ]
      opComparisonsMap =
        [ (">", COMPARATOR COMPARE_GT)
        , ("<", COMPARATOR COMPARE_LT)
        , ("==", COMPARATOR COMPARE_EQ)
        , ("!=", COMPARATOR COMPARE_NE)
        , (">=", COMPARATOR COMPARE_GE)
        , ("<=", COMPARATOR COMPARE_LE)
        ]
      op1Instruction = case lookup op1 opCalculationsMap of
        Just opCalc -> [opCalc]
        Nothing -> case lookup op1 opComparisonsMap of
          Just opComp -> [opComp]
          Nothing -> error ("Unsupported operation: " ++ op1)
      op2Instruction = case lookup op2 opCalculationsMap of
        Just opCalc -> [opCalc]
        Nothing -> case lookup op2 opComparisonsMap of
          Just opComp -> [opComp]
          Nothing -> error ("Unsupported operation: " ++ op2)
      isOp1Calculation = op1 `elem` map fst opCalculationsMap
  in
    if isOp1Calculation
    then
      (leftInstructions ++ middleInstructions ++ op1Instruction ++ rightInstructions ++ op2Instruction, state3)
    else
      (middleInstructions ++ rightInstructions ++ op2Instruction ++ leftInstructions ++ op1Instruction, state3)



-- Visit AST
generateBytecode (SList ast) state = 
  let (instructions, newState) = foldl  (\(accInstr, accState) astElem ->
                                          let (instr, newState') = generateBytecode astElem accState
                                          in (accInstr ++ instr, newState')
                                        ) ([], state) ast
  in (instructions, newState)

-- Store variables
generateBytecode (SDefine name (SType _) value) state =
  let (valueInstructions, newState) = generateBytecode value state
  in (valueInstructions ++ [STORE_VAR name], newState)

-- Load variables
generateBytecode (SVariable var) state = ([LOAD_VAR var], state)

-- Store constants
generateBytecode (SInt n) state = ([STORE_CONST (VInt n)], state)
generateBytecode (SFloat f) state = ([STORE_CONST (VFloat f)], state)
generateBytecode (SBool b) state = ([STORE_CONST (VBool b)], state)
generateBytecode (SString s) state = ([STORE_CONST (VString s)], state)
generateBytecode (SChar c) state = ([STORE_CONST (VChar c)], state)

-- Unsupported cases
generateBytecode _ state = ([], state)

generateInstructionsList :: AST -> [Instruction]
generateInstructionsList ast =
  let initialState = LabelState 0 0
      (instructions, _) = generateBytecode ast initialState
  in instructions
