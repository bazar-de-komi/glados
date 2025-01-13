module Execute (execute) where

-- import Data.Either (either)
import Data.Map (Map)
import qualified Data.Map as Map

data Val
  = IntVal Int
  | FloatVal Float
  | StringVal String
  | CharVal Char
  | BoolVal Bool
  deriving (Show, Eq)

data BynaryOperator
  = ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | MODULO

data BynaryComparator
  = COMPARE_GT
  | COMPARE_LT
  | COMPARE_EQ
  | COMPARE_NE
  | COMPARE_GE
  | COMPARE_LE

data Instruction
  = STORE_CONST Value
  | STORE_VAR String
  | LOAD_VAR String
  | OPERATOR BynaryOperator
  | COMPARATOR BynaryComparator
  | JUMP String
  | JUMP_IF_FALSE String
  | LABEL String
  | LABEL_FUNC String
  | LABEL_FUNC_END String
  | CALL String
  | RETURN
  | HALT
  deriving (Show, Eq)

-- data Function = Function
--   {
--     fName :: String,
--     fArgs :: [Val],
--     fLines :: [String]
--   } deriving (Show)

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
    Just inst -> Left "Need to create execute inst function"
    Nothing -> Left $ "Invalid inst: " ++ line

parseInst :: [String] -> Maybe Instruction
parseInst ("STORE_CONST" : val : _) = Just (STORE_CONST (parseVal val))
parseInst ("STORE_VAR" : name : _) = Just (STORE_VAR (stripQuotes name))
parseInst ("LOAD_VAR" : name : _) = Just (LOAD_VAR (stripQuotes name))
parseInst ("ADD" : _) = Just ADD
parseInst ("SUBTRACT" : _) = Just SUBTRACT
parseInst ("MULTIPLY" : _) = Just MULTIPLY
parseInst ("DIVIDE" : _) = Just DIVIDE
parseInst ("MODULO" : _) = Just MODULO
parseInst ("COMPARE_GT" : _) = Just COMPARE_GT
parseInst ("COMPARE_LT" : _) = Just COMPARE_LT
parseInst ("COMPARE_EQ" : _) = Just COMPARE_EQ
parseInst ("COMPARE_NE" : _) = Just COMPARE_NE
parseInst ("COMPARE_GE" : _) = Just COMPARE_GE
parseInst ("COMPARE_LE" : _) = Just COMPARE_LE
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
