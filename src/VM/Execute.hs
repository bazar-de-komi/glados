module VM.Execute (initialState, execute, parseInst, stripQuotes,
  parseVal, executeInstruction) where

import Data.Either (either)
import qualified Data.Map as Map
import VM.VMData (Value, BinaryOperator, BinaryComparator, Instruction, VM)
import VM.Instructions (handleStoreConst, handleStoreVar, handleLoadVar,
  handleOperator, handleComparator, handleJump, handleJumpIfFalse,
  handleCall, handleReturn, handleHalt)

initialState :: VM
initialState = VM
  {
    stack = [],
    variables = Map.empty,
    program = [],
    index = 0,
    labels = Map.empty
  }

execute :: String -> VM -> Either String VM
execute line vm =
  case parseInst (words line) of
    Just inst -> Right $ executeInstruction vm inst
    Nothing -> Left $ "Invalid inst: " ++ line

parseInst :: [String] -> Either String Instruction
parseInst ("STORE_CONST" : val : _) =
  Right (STORE_CONST (parseVal val))
parseInst ("STORE_VAR" : name : _) =
  Right (STORE_VAR (stripQuotes name))
parseInst ("LOAD_VAR" : name : _) =
  Right (LOAD_VAR (stripQuotes name))
parseInst ("ADD" : _) =
  Right (OPERATOR ADD)
parseInst ("SUBTRACT" : _) =
  Right (OPERATOR SUBTRACT)
parseInst ("MULTIPLY" : _) =
  Right (OPERATOR MULTIPLY)
parseInst ("DIVIDE" : _) =
  Right (OPERATOR DIVIDE)
parseInst ("MODULO" : _) =
  Right (OPERATOR MODULO)
parseInst ("COMPARE_GT" : _) =
  Right (COMPARATOR COMPARE_GT)
parseInst ("COMPARE_LT" : _) =
  Right (COMPARATOR COMPARE_LT)
parseInst ("COMPARE_EQ" : _) =
  Right (COMPARATOR COMPARE_EQ)
parseInst ("COMPARE_NE" : _) =
  Right (COMPARATOR COMPARE_NE)
parseInst ("COMPARE_GE" : _) =
  Right (COMPARATOR COMPARE_GE)
parseInst ("COMPARE_LE" : _) =
  Right (COMPARATOR COMPARE_LE)
parseInst ("JUMP" : label : _) =
  Right (JUMP (stripQuotes label))
parseInst ("JUMP_IF_FALSE" : label : _) =
  Right (JUMP_IF_FALSE (stripQuotes label))
parseInst ("LABEL" : name : _) =
  Right (LABEL (stripQuotes name))
parseInst ("CALL" : name : _) =
  Right (CALL (stripQuotes name))
parseInst ("RETURN" : _) =
  Right RETURN
parseInst ("HALT" : _) =
  Right HALT
parseInst [] =
  Left "Error: Empty intruction line."
parseInst _ =
  Left "Error: Unknown instuction"

stripQuotes :: String -> String
stripQuotes str =
  if head str == '"' && last str == '"'
  then init (tail str)
  else str

parseVal :: String -> Value
parseVal val
  | val == "True" = BoolVal True
  | val == "False" = BoolVal False
  | head val == '"' = StringVal (init (tail val))
  | '.' `elem` val = FloatVal (read val)
  | otherwise = IntVal (read val)

executeInstruction :: VM -> Instruction -> Either String VM
executeInstruction vm (STORE_CONST val) = handleStoreConst vm val
executeInstruction vm (STORE_VAR name) = handleStoreVar vm name
executeInstruction vm (LOAD_VAR name) = handleLoadVar vm name
executeInstruction vm (OPERATOR op) =
  | length (stack vm) < 2 = Left "Error: Operator requires two values on the stack."
  | case stack vm of
    (IntVal x : IntVal y : rest) -> handleOperator x y op
  | otherwise = Left "Error: Operator requires two integers on the stack."
executeInstruction vm (COMPARATOR comp) =
  | length (stack vm) < 2 = Left "Error: Comparator requires two values on the stack."
  | case stack vm of
    (IntVal x : IntVal y : rest) -> handleComparator x y comp
  | otherwise = Left "Error: Comparator requires two values on the stack."
executeInstruction vm (JUMP label) = handleJump vm label
executeInstruction vm (JUMP_IF_FALSE label) = handleJumpIfFalse vm label
executeInstruction vm (CALL label) = handleCall vm label
executeInstruction vm RETURN = handleReturn vm
executeInstruction vm HALT = handleHalt vm
