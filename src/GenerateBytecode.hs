{-# LANGUAGE LambdaCase #-}

module GenerateBytecode
  ( generateInstructionsList,
  )
where

import Structure (AST(..), Instruction(..), BinaryOperator (..), BinaryComparator (..), Value(..), LabelState(..))

generateLabel :: LabelState -> String -> (String, LabelState)
generateLabel (LabelState loop ifCount) labelType =
  case labelType of
    "loop" -> let newState = LabelState (loop + 1) ifCount
              in ("loop_" ++ show loop, newState)
    "if"   -> let newState = LabelState loop (ifCount + 1)
              in ("if_" ++ show ifCount, newState)
    _      -> error "Unknown label type"

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

-- If
generateBytecode (SIf condition thenBranch elseBranch) state =
  let (elseLabel, state1) = generateLabel state "if"
      (endLabel, state2) = generateLabel state1 "if"
      (conditionInstructions, state3) = generateBytecode condition state2
      (thenInstructions, state4) = generateBytecode thenBranch state3
      (elseInstructions, state5) = generateBytecode elseBranch state4
  in (
    conditionInstructions ++
    [JUMP_IF_FALSE elseLabel] ++
    thenInstructions ++
    [JUMP endLabel, LABEL elseLabel] ++
    elseInstructions ++
    [LABEL endLabel],
    state5
  )

-- For loop
generateBytecode (SFor initExpr condExpr updateExpr body) state =
  let (initInstrs, initState) = generateBytecode initExpr state
      (loopCondLabel, loopState1) = generateLabel initState "loop"
      (loopEndLabel, loopState2) = generateLabel loopState1 "loop"
      (condInstrs, condState) = generateBytecode condExpr loopState2
      condJumpInstr = [JUMP_IF_FALSE loopEndLabel]
      (bodyInstrs, bodyState) = generateBytecode body condState
      (updateInstrs, finalState) = generateBytecode updateExpr bodyState
  in (
    initInstrs ++
    [LABEL loopCondLabel] ++
    condInstrs ++
    condJumpInstr ++
    bodyInstrs ++
    updateInstrs ++
    [JUMP loopCondLabel] ++
    [LABEL loopEndLabel], finalState
  )

-- While loop
generateBytecode (SLoop condExpr body) state =
  let (loopCondLabel, loopState1) = generateLabel state "loop"
      (loopEndLabel, loopState2) = generateLabel loopState1 "loop"
      (condInstrs, condState) = generateBytecode condExpr loopState2
      condJumpInstr = [JUMP_IF_FALSE loopEndLabel]
      (bodyInstrs, finalState) = generateBytecode body condState
  in (
    [LABEL loopCondLabel] ++
    condInstrs ++
    condJumpInstr ++
    bodyInstrs ++
    [JUMP loopCondLabel] ++
    [LABEL loopEndLabel], finalState
  )

-- SFunc
generateBytecode (SFunc name (SType _) params body) state =
  let funcLabel = name
      paramInstructions = case params of
        SList ps -> 
          let processParams [] = []
              processParams (SType _:SList vars:rest) =
                let varsInstructions =
                      concatMap (\case
                        SVariable param -> [STORE_VAR param]
                        _ -> error "Unsupported parameter format in vars"
                      ) vars
                in varsInstructions ++ processParams rest
              processParams _ = error "Invalid parameter format in function definition"
          in processParams ps
        _ -> error "Invalid parameter format in function definition"
      (bodyInstructions, newState) = generateBytecode body state
  in (
    [LABEL_FUNC funcLabel] ++
    paramInstructions ++
    bodyInstructions ++
    [LABEL_FUNC_END funcLabel],
    newState
  )

-- Return
generateBytecode (SReturn returnValue) state =
  let (returnInstructions, newState) = generateBytecode returnValue state
  in (
    returnInstructions ++
    [RETURN],
    newState
  )

-- Function Call
generateBytecode (SCall funcName (SList args)) state =
  let
    (argInstructions, newState) = foldl
      (\(accInstr, accState) arg ->
        let (argInstr, nextState) = generateBytecode arg accState
        in (argInstr ++ accInstr, nextState))
      ([], state)
      args
  in 
    (argInstructions ++ [CALL funcName], newState)

-- Unsupported cases
generateBytecode _ state = ([], state)

generateInstructionsList :: AST -> [Instruction]
generateInstructionsList ast =
  let initialState = LabelState 0 0
      (instructions, _) = generateBytecode ast initialState
  in instructions
