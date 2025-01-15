-- | Data structures for S-Expressions and Abstract Syntax Trees (ASTs).
--
-- This module defines:
--
-- 1. The `SExpr` type for representing Kleftis symbolic expressions (S-Expressions).
-- 2. The `AST` type for representing parsed and processed Abstract Syntax Trees.

module Structure (SExpr(..), AST(..), Instruction (..), BinaryOperator (..), BinaryComparator (..), Value(..), LabelState(..), VM (..)) where

import qualified Data.Map as Map

-- | Represents a Kleftis S-Expression (SExpr).
--
-- S-Expressions can represent atomic values, lists, or more complex constructs
-- such as conditionals, loops, or function calls.
data SExpr
  = Atom String         -- ^ Represents a single atomic value (e.g., a variable or symbol).
  | SEInt Int           -- ^ Represents an integer value.
  | SEChar Char         -- ^ Represents a character value.
  | SEString String     -- ^ Represents a string value.
  | SEFloat Float       -- ^ Represents a floating-point value.
  | Boolean Bool        -- ^ Represents a boolean value (`True` or `False`).
  | Type String         -- ^ Represents a type annotation (e.g., `int`, `float`).
  | BasicFunc String    -- ^ Represents a basic function or operation (e.g., `+`, `-`, `*`).
  | Param [SExpr]       -- ^ Represents a list of parameters in a function or expression.
  | SEIf SExpr SExpr SExpr
      -- ^ Represents a conditional `if` expression with:
      -- 1. A condition (the first `SExpr`),
      -- 2. A `then` branch (the second `SExpr`),
      -- 3. An `else` branch (the third `SExpr`).
  | SELoop SExpr SExpr
      -- ^ Represents a `while` loop with:
      -- 1. A condition (the first `SExpr`),
      -- 2. A body (the second `SExpr`).
  | SEFor SExpr SExpr SExpr SExpr
      -- ^ Represents a `for` loop with:
      -- 1. Initialization (the first `SExpr`),
      -- 2. A condition (the second `SExpr`),
      -- 3. An update (the third `SExpr`),
      -- 4. A body (the fourth `SExpr`).
  | SEList [SExpr]      -- ^ Represents a list of symbolic expressions.
  | List [SExpr]        -- ^ Represents a generic list of symbolic expressions.
  | Return SExpr        -- ^ Represents a return expression with a single value.
  deriving (Eq, Show)

-- | Represents an Abstract Syntax Tree (AST) for a Kleftis programming language.
--
-- The AST models various constructs of the language, such as variables, functions,
-- loops, and conditionals.
data AST
  = SBool Bool          -- ^ Represents a boolean value (`True` or `False`).
  | SFloat Float        -- ^ Represents a floating-point value.
  | SInt Int            -- ^ Represents an integer value.
  | SChar Char          -- ^ Represents a character value.
  | SType String        -- ^ Represents a type annotation (e.g., `int`, `float`).
  | SString String      -- ^ Represents a string value.
  | SVariable String    -- ^ Represents a variable or symbol.
  | SOperation String   -- ^ Represents an operation (e.g., `+`, `-`, `*`).
  | SCall String AST
      -- ^ Represents a function call with:
      -- 1. The function name (`String`),
      -- 2. The function's parameters (`AST`),
  | SFunc String AST AST AST
      -- ^ Represents a function definition with:
      -- 1. The function name (`String`),
      -- 2. The function's type (`AST`),
      -- 3. The function's parameters (`AST`),
      -- 4. The function's body (`AST`).
  | SDefine String AST AST
      -- ^ Represents a definition (variable or function without parameters) with:
      -- 1. The name (`String`),
      -- 2. The type (`AST`),
      -- 3. The body or value (`AST`).
  | SReturn AST
      -- ^ Represents a return expression containing a single `AST`.
  | SLoop AST AST
      -- ^ Represents a `while` loop with:
      -- 1. The loop condition (`AST`),
      -- 2. The loop body (`AST`).
  | SFor AST AST AST AST
      -- ^ Represents a `for` loop with:
      -- 1. Initialization (`AST`),
      -- 2. Condition (`AST`),
      -- 3. Update (`AST`),
      -- 4. Body (`AST`).
  | SIf AST AST AST
      -- ^ Represents a conditional `if` expression with:
      -- 1. A condition (`AST`),
      -- 2. A `then` branch (`AST`),
      -- 3. An `else` branch (`AST`).
  | SListOf [AST]
      -- ^ Represents a list of multiple AST nodes.
  | SList [AST]
      -- ^ Represents a list of AST nodes.
  deriving (Eq, Show)

-- | Represents the types of values that can be manipulated by the virtual machine.
data Value
  = VInt Int        -- ^ Integer value.
  | VFloat Float    -- ^ Floating-point value.
  | VBool Bool      -- ^ Boolean value.
  | VString String  -- ^ String value.
  | VChar Char      -- ^ Character value.
  deriving (Eq)

instance Show Value where
-- | Displays the `Value` in a human-readable format.
  show (VInt int) = show int
  show (VBool bool) = show bool
  show (VFloat float) = show float
  show (VString string) = show string
  show (VChar char) = show char

-- | Represents binary arithmetic operators supported by the virtual machine.
data BinaryOperator
  = ADD       -- ^ Addition operator.
  | SUBTRACT  -- ^ Subtraction operator.
  | MULTIPLY  -- ^ Multiplication operator.
  | DIVIDE    -- ^ Division operator.
  | MODULO    -- ^ Modulus operator.
  deriving (Show, Eq)

-- | Represents binary comparison operators supported by the virtual machine.
data BinaryComparator
  = COMPARE_GT  -- ^ Greater than operator.
  | COMPARE_LT  -- ^ Less than operator.
  | COMPARE_EQ  -- ^ Equal to operator.
  | COMPARE_NE  -- ^ Not equal to operator.
  | COMPARE_GE  -- ^ Greater than or equal to operator.
  | COMPARE_LE  -- ^ Less than or equal to operator.
  deriving (Show, Eq)

-- | Represents a single instruction in the virtual machine.
data Instruction
  = STORE_CONST Value       -- ^ Pushes a constant value onto the stack.
  | STORE_VAR String        -- ^ Stores the top value of the stack into a variable and remove it from the stack.
  | LOAD_VAR String         -- ^ Loads a variable's value onto the stack.
  | OPERATOR BinaryOperator -- ^ Applies a binary arithmetic operation on the top two stack values.
  | COMPARATOR BinaryComparator -- ^ Applies a binary comparison operation on the top two stack values.
  | JUMP String             -- ^ Jumps to a specified label unconditionally.
  | JUMP_IF_FALSE String    -- ^ Jumps to a specified label if the top stack value is `False`.
  | LABEL String            -- ^ Marks a label in the instruction sequence.
  | LABEL_FUNC String       -- ^ Marks the beginning of a function with a label.
  | LABEL_FUNC_END String   -- ^ Marks the end of a function with a label.
  | CALL String             -- ^ Calls a function identified by a label.
  | RETURN                  -- ^ Returns from the current function.
  | HALT                    -- ^ Stops the execution of the virtual machine.
  deriving (Show, Eq)

-- | Represents the state for generating unique labels in loops and conditions.
data LabelState = LabelState 
  { loopCounter :: Int  -- ^ Counter for generating unique loop labels.
  , ifCounter :: Int    -- ^ Counter for generating unique condition labels.
  }

-- | Represents the state of the virtual machine.
data VM = VM
  { stack :: [Value]                    -- ^ The stack of the virtual machine, holding `Value` elements.
  , variables :: Map.Map String Value   -- ^ The map of variable names to their corresponding values.
  , index :: Int                        -- ^ The index of the currently executing instruction.
  , indexBeforeFuncCall :: Maybe Int   -- ^ The instruction index before the last function call, if any.
  , instructions :: [Instruction]      -- ^ The list of instructions loaded in the virtual machine.
  }

instance Show VM where
  -- | Displays the first value of the stack if present, or a default message if the stack is empty.
  show vm =
    case stack vm of
      (val:_) -> show val
      []      -> "No return value"
