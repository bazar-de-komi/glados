module Instruction (handleStoreConst, handleStoreVar, handleLoadVar, handleOperator, handleComparator, handleJump, handleJumpIfFalse, handleCall, handleReturn, handleHalt) where

import Execute (Val, BinaryOperator, BinaryComparator, Instruction, VM)

handleStoreConst :: VM -> Value -> Either String VM
handleStoreConst vm val =
  Right vm { stack = value : stack }

handleStoreVar :: VM -> String -> Either String VM
handleStoreVar vm name =
  case stack vm of
    (val:rest) -> Right vm { stack = rest, variables = Map.insert name val (variables vm) }
    _ -> Left "Error: Stack underflow on STORE_VAR."

handleLoadVar :: VM -> String -> Either String VM
handleLoadVar vm name =
  case Map.lookup name (variables vm) of
    Just val -> Right vm { stack = storeConst val (stack vm) }
    Nothing -> Left $ "Error: Variable not found: " ++ name

handleOperator :: VM -> Operator -> Either String VM
handleOperator vm op =
  case stack vm of
    (IntVal x : IntVal y : rest) ->
      let result = case op of
            ADD      -> y + x
            SUBTRACT -> y - x
            MULTIPLY -> y * x
            DIVIDE   -> if x /= 0 then y `div` x else error "Division by zero"
            MODULO   -> if x /= 0 then y `mod` x else error "Modulo by zero"
      in Right vm { stack = storeConst (IntVal result) rest }
    _ -> Left "Error: Operator requires two integers on the stack."

handleComparator :: VM -> Comparator -> Either String VM
handleComparator vm comp =
  case stack vm of
    (IntVal x : IntVal y : rest) ->
      let result = case comp of
            COMPARE_GT -> y > x
            COMPARE_LT -> y < x
            COMPARE_EQ -> y == x
            COMPARE_NE -> y /= x
            COMPARE_GE -> y >= x
            COMPARE_LE -> y <= x
      in Right vm { stack = storeConst (BoolVal result) rest }
    _ -> Left "Error: Comparator requires two integers on the stack."

handleJump :: VM -> String -> Either String VM
handleJump vm label =
  case Map.lookup label (labels vm) of
    Just idx -> Right vm { index = idx }
    Nothing -> Left $ "Error: Label not found: " ++ label

handleJumpIfFalse :: VM -> String -> Either String VM
handleJumpIfFalse vm label =
  case stack vm of
    (BoolVal cond : rest) ->
      if not cond
      then case Map.lookup label (labels vm) of
             Just idx -> Right vm { index = idx, stack = rest }
             Nothing -> Left $ "Error: Label not found: " ++ label
      else Right vm { stack = rest }
    _ -> Left "Error: JUMP_IF_FALSE requires a boolean on the stack."

handleCall :: VM -> String -> Either String VM
handleCall vm label =
  case Map.lookup label (labels vm) of
    Just idx -> Right vm { stack = storeConst (IntVal (index vm)) (stack vm), index = idx }
    Nothing -> Left $ "Error: Function label not found: " ++ label

handleReturn :: VM -> Either String VM
handleReturn vm =
  case stack vm of
    (IntVal retIdx : rest) -> Right vm { stack = rest, index = retIdx }
    _ -> Left "Error: RETURN requires a return address on the stack."

handleHalt :: VM -> Either String VM
handleHalt vm =
  Right vm { program = [], index = -1 }
