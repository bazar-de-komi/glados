module Execute (execute) where

-- import Data.Either (either)
import Data.Map (Map)
import qualified Data.Map as Map
import

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

parseInst :: [String] -> Maybe Instruction
parseInst ("STORE_CONST" : val : _) = Just (STORE_CONST (parseVal val))
parseInst ("STORE_VAR" : name : _) = Just (STORE_VAR (stripQuotes name))
parseInst ("LOAD_VAR" : name : _) = Just (LOAD_VAR (stripQuotes name))
parseInst ("ADD" : _) = Just (OPERATOR ADD)
parseInst ("SUBTRACT" : _) = Just (OPERATOR SUBTRACT)
parseInst ("MULTIPLY" : _) = Just (OPERATOR MULTIPLY)
parseInst ("DIVIDE" : _) = Just (OPERATOR DIVIDE)
parseInst ("MODULO" : _) = Just (OPERATOR MODULO)
parseInst ("COMPARE_GT" : _) = Just (COMPARATOR COMPARE_GT)
parseInst ("COMPARE_LT" : _) = Just (COMPARATOR COMPARE_LT)
parseInst ("COMPARE_EQ" : _) = Just (COMPARATOR COMPARE_EQ)
parseInst ("COMPARE_NE" : _) = Just (COMPARATOR COMPARE_NE)
parseInst ("COMPARE_GE" : _) = Just (COMPARATOR COMPARE_GE)
parseInst ("COMPARE_LE" : _) = Just (COMPARATOR COMPARE_LE)
parseInst ("JUMP" : label : _) = Just (JUMP (stripQuotes label))
parseInst ("JUMP_IF_FALSE" : label : _) = Just (JUMP_IF_FALSE (stripQuotes label))
parseInst ("LABEL" : name : _) = Just (LABEL (stripQuotes name))
parseInst ("CALL" : name : _) = Just (CALL (stripQuotes name))
parseInst ("RETURN" : _) = Just RETURN
parseInst ("HALT" : _) = Just HALT
parseInst _ = Nothing

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
executeInstruction vm (STORE_CONST val) =
  Right vm { stack = push val (stack vm) }
-- les autres doivent encore être modifiées
executeInstruction vm (STORE_VAR name) =
  case stack vm of
    (val:rest) -> Right vm { stack = rest, variables = Map.insert name val (variables vm) }
    _ -> Left "Stack underflow on STORE_VAR."
executeInstruction vm (LOAD_VAR name) =
  case Map.lookup name (variables vm) of
    Just val -> Right vm { stack = val : stack vm }
    Nothing -> Left $ "Variable not found: " ++ name
executeInstruction vm ADD =
  case stack vm of
    (IntVal x : IntVal y : rest) -> Right vm { stack = IntVal (x + y) : rest }
    _ -> Left "Error: the function ADD requires two integers on the stack."
-- add les autres instructions 
executeInstruction vm HALT =
  Left "HALT: Execution stopped."
executeInstruction _ _ = Left "Unhandled instruction."
