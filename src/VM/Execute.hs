module Execute (execute, Val, BinaryOperator, BinaryComparator, Instruction, VM) where

-- import Data.Either (either)
import Data.Map (Map)
import qualified Data.Map as Map
import Instructions (handleStoreConst, handleStoreVar, handleLoadVar, handleOperator, handleComparator, handleJump, handleJumpIfFalse, handleCall, handleReturn, handleHalt)

data Val
  = IntVal Int
  | FloatVal Float
  | StringVal String
  | CharVal Char
  | BoolVal Bool
  deriving (Show, Eq)

data BinaryOperator
  = ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | MODULO
  deriving (Show, Eq)

data BinaryComparator
  = COMPARE_GT
  | COMPARE_LT
  | COMPARE_EQ
  | COMPARE_NE
  | COMPARE_GE
  | COMPARE_LE
  deriving (Show, Eq)

data Instruction
  = STORE_CONST Value
  | STORE_VAR String
  | LOAD_VAR String
  | OPERATOR BinaryOperator
  | COMPARATOR BinaryComparator
  | JUMP String
  | JUMP_IF_FALSE String
  | LABEL String
  | LABEL_FUNC String
  | LABEL_FUNC_END String
  | CALL String
  | RETURN
  | HALT
  deriving (Show, Eq)

data VM = VM
  {
    stack :: [Val],
    variables :: Map String Val,
    program :: [Inst],
    index :: Int,
    labels :: Map String Int
  } deriving (Show)

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
    Just inst -> Right $ executeInst vm inst
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

parseVal :: String -> Val
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
executeInstruction vm (OPERATOR op) = handleOperator vm op
executeInstruction vm (COMPARATOR comp) = handleComparator vm comp
executeInstruction vm (JUMP label) = handleJump vm label
executeInstruction vm (JUMP_IF_FALSE label) = handleJumpIfFalse vm label
executeInstruction vm (CALL label) = handleCall vm label
executeInstruction vm RETURN = handleReturn vm
executeInstruction vm HALT = handleHalt vm
