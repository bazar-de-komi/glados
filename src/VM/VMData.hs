module VM.VMData (Value, BinaryOperator, BinaryComparator, Instruction, VM) where

import qualified Data.Map as Map

data Value
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
    stack :: [Value],
    variables :: Map.Map String Value,
    program :: [Instruction],
    index :: Int,
    labels :: Map.Map String Int
  } deriving (Show)
