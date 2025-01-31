-- | This module provides the implementation of a virtual machine (VM) capable of executing a custom bytecode language.
-- It includes functions for initializing the VM, handling execution of various instructions, managing control flow,
-- variable storage, arithmetic and logical operations, function calls, and program termination.

{-# LANGUAGE NamedFieldPuns #-}

module VM.RunVM (initializeVM, findLabel, execute, executeInstructions, runVM) where

import qualified Data.Map as Map
import Structure (Value (..), BinaryOperator (..), BinaryComparator (..), Instruction (..), VM (..))
import Data.List (find)

-- | Initializes a virtual machine (VM) with a given list of instructions.
--
-- ==== Parameters
-- * `[Instruction]`: A list of bytecode instructions that the VM will execute.
--
-- ==== Returns
-- * `VM`: A virtual machine instance with an empty stack, an empty variable map,
--   and the program counter (`index`) set to 0.
--
-- ==== Examples
-- >>> initializeVM [STORE_CONST (VInt 42), HALT]
-- VM {stack = [], variables = fromList [], index = 0, indexBeforeFuncCall = Nothing, instructions = [STORE_CONST (VInt 42), HALT]}
initializeVM :: [Instruction] -> VM
initializeVM instrs = VM
  { stack = []
  , variables = Map.empty
  , index = 0
  , callStack = []
  , instructions = instrs
  }

-- | Finds the index of a label in a list of instructions.
--
-- ==== Parameters
-- * `Instruction`: The origin instruction.
-- * `[Instruction]`: The list of instructions to search within.
--
-- ==== Returns
-- * `Maybe Int`: The index of the instruction containing the label, or `Nothing` if not found.
--
-- ==== Examples
-- >>> findLabel (LABEL_FUNC "func") [LABEL_FUNC "func", LABEL_FUNC_END "func", STORE_CONST (VInt 42)]
-- Just 1
-- >>> findLabel (CALL "func") [CALL "func", LABEL_FUNC "func", LABEL_FUNC_END "func", STORE_CONST (VInt 42)]
-- Just 1
-- >>> findLabel (JUMP "label") [JUMP "label", LABEL "label", STORE_CONST (VInt 42)]
-- Just 1
-- >>> findLabel (JUMP_IF_FALSE "label") [JUMP_IF_FALSE "label", LABEL "label", STORE_CONST (VInt 42)]
-- Just 1
-- >>> findLabel (CALL "nonexistent") [STORE_CONST (VInt 42), HALT]
-- Nothing
-- >>> findLabel (JUMP_IF_FALSE "nonexistent") [STORE_CONST (VInt 42), HALT]
-- Nothing
-- >>> findLabel (JUMP "nonexistent") [STORE_CONST (VInt 42), HALT]
-- Nothing
findLabel :: Instruction -> [Instruction] -> Maybe Int
findLabel instr program =
  let labels = zip [0..] program
  in case instr of
      LABEL_FUNC label -> fmap fst (find (\(_, currentInstr) -> case currentInstr of
                                                    LABEL_FUNC_END endLabel -> endLabel == label
                                                    _ -> False) labels)
      CALL label -> fmap fst (find (\(_, currentInstr) -> case currentInstr of
                                                    LABEL_FUNC funcLabel -> funcLabel == label
                                                    _ -> False) labels)
      JUMP label -> fmap fst (find (\(_, currentInstr) -> case currentInstr of
                                                    LABEL label' -> label' == label
                                                    _ -> False) labels)
      JUMP_IF_FALSE label -> fmap fst (find (\(_, currentInstr) -> case currentInstr of
                                                    LABEL label' -> label' == label
                                                    _ -> False) labels)
      _ -> Nothing

-- | Executes a single instruction on the virtual machine.
--
-- ==== Parameters
-- * `Instruction`: The instruction to execute.
-- * `VM`: The current state of the virtual machine.
--
-- ==== Returns
-- * `VM`: The updated state of the virtual machine after executing the instruction.
--
-- ==== Logic
-- Cette fonction est divisée en plusieurs cas, chacun correspondant à un type d'instruction :
-- 1. **STORE_CONST** : Empile une valeur sur la pile.
-- 2. **STORE_VAR** : Stocke une valeur de la pile dans une variable.
-- 3. **LOAD_VAR** : Charge une variable dans la pile.
-- 4. **OPERATOR** : Exécute des opérations binaires (addition, soustraction, etc.).
-- 5. **COMPARATOR** : Effectue des comparaisons (>, <, ==, etc.).
-- 6. **CALL** : Appelle une fonction en changeant l'index.
-- 7. **LABEL_FUNC** / **LABEL_FUNC_END** : Gère les étiquettes de fonction.
-- 8. **JUMP** / **JUMP_IF_FALSE** : Gère les sauts conditionnels ou inconditionnels.
-- 9. **RETURN** : Revient à l'instruction après un appel de fonction.
-- 10. **HALT** : Arrête la VM.
--
-- ==== Examples
-- >>> let vm = initializeVM [STORE_CONST (VInt 42)]
-- >>> execute (STORE_CONST (VInt 42)) vm
-- VM {stack = [VInt 42], variables = fromList [], index = 1, indexBeforeFuncCall = Nothing, instructions = [...]}
execute :: Instruction -> VM -> VM

-- Execute STORE_CONST
execute (STORE_CONST val) vm =
  let newStack = val : stack vm
  in vm { stack = newStack, index = index vm + 1 }

-- Execute STORE_VAR
execute (STORE_VAR name) vm =
  case stack vm of
    (val:rest) -> 
      let newVariables = Map.insert name val (variables vm)
      in vm { variables = newVariables, stack = rest, index = index vm + 1 }
    [] -> error "Stack underflow in STORE_VAR"

-- Execute LOAD_VAR
execute (LOAD_VAR name) vm =
  case Map.lookup name (variables vm) of
    Just val -> vm { stack = val : stack vm, index = index vm + 1 }
    Nothing -> error $ "Variable not found: " ++ name

-- Execute OPERATOR
execute (OPERATOR op) vm =
  case stack vm of
    (VInt x : VInt y : rest) ->
      let result = case op of
                    ADD -> VInt (y + x)
                    SUBTRACT -> VInt (y - x)
                    MULTIPLY -> VInt (y * x)
                    DIVIDE -> if x /= 0 then VInt (y `div` x) else error "Division by zero"
                    MODULO -> if x /= 0 then VInt (y `mod` x) else error "Modulo by zero"
      in vm { stack = result : rest, index = index vm + 1 }
    (VFloat x : VFloat y : rest) ->
      let result = case op of
                    ADD -> VFloat (y + x)
                    SUBTRACT -> VFloat (y - x)
                    MULTIPLY -> VFloat (y * x)
                    DIVIDE -> if x /= 0 then VFloat (y / x) else error "Division by zero"
                    MODULO -> error "Modulo operation not supported for floats"
      in vm { stack = result : rest, index = index vm + 1 }
    (VString x : VString y : rest) ->
      let result = case op of
                    ADD -> VString (y ++ x)
                    _ -> error "Operations other than addition are not supported for string"
      in vm { stack = result : rest, index = index vm + 1 }
    _ -> error "Stack underflow or invalid types in OPERATOR"

-- Execute COMPARATOR
execute (COMPARATOR cmp) vm =
  case stack vm of
    (VInt x : VInt y : rest) ->
      let result = case cmp of
                    COMPARE_GT -> VBool (y > x)
                    COMPARE_LT -> VBool (y < x)
                    COMPARE_EQ -> VBool (y == x)
                    COMPARE_NE -> VBool (y /= x)
                    COMPARE_GE -> VBool (y >= x)
                    COMPARE_LE -> VBool (y <= x)
      in vm { stack = result : rest, index = index vm + 1 }
    (VFloat x : VFloat y : rest) ->
      let result = case cmp of
                    COMPARE_GT -> VBool (y > x)
                    COMPARE_LT -> VBool (y < x)
                    COMPARE_EQ -> VBool (y == x)
                    COMPARE_NE -> VBool (y /= x)
                    COMPARE_GE -> VBool (y >= x)
                    COMPARE_LE -> VBool (y <= x)
      in vm { stack = result : rest, index = index vm + 1 }
    (VString x : VString y : rest) ->
      let result = case cmp of
                    COMPARE_GT -> VBool (y > x)
                    COMPARE_LT -> VBool (y < x)
                    COMPARE_EQ -> VBool (y == x)
                    COMPARE_NE -> VBool (y /= x)
                    COMPARE_GE -> VBool (y >= x)
                    COMPARE_LE -> VBool (y <= x)
      in vm { stack = result : rest, index = index vm + 1 }
    (VChar x : VChar y : rest) ->
      let result = case cmp of
                    COMPARE_GT -> VBool (y > x)
                    COMPARE_LT -> VBool (y < x)
                    COMPARE_EQ -> VBool (y == x)
                    COMPARE_NE -> VBool (y /= x)
                    COMPARE_GE -> VBool (y >= x)
                    COMPARE_LE -> VBool (y <= x)
      in vm { stack = result : rest, index = index vm + 1 }
    (VBool x : VBool y : rest) ->
      let result = case cmp of
                    COMPARE_EQ -> VBool (x == y)
                    COMPARE_NE -> VBool (x /= y)
                    _ -> error "Invalid comparison for booleans"
      in vm { stack = result : rest, index = index vm + 1 }
    _ -> error "Stack underflow or invalid types in COMPARATOR"

-- Execute CALL
execute (CALL funcLabel) vm =
  case findLabel (CALL funcLabel) (instructions vm) of
    Just funcIndex ->
      vm { callStack = index vm + 1 : callStack vm, index = funcIndex }
    Nothing -> error $ "Function label not found: " ++ funcLabel

-- Execute LABEL_FUNC
execute (LABEL_FUNC funcName) vm =
  case callStack vm of
    (_ : _) -> vm { index = index vm + 1 }
    [] ->
      case findLabel (LABEL_FUNC funcName) (instructions vm) of
        Just endIndex -> vm { index = endIndex + 1 }
        Nothing -> error $ "LABEL_FUNC_END not found for " ++ funcName

-- Execute LABEL_FUNC_END
execute (LABEL_FUNC_END _) vm =
  case callStack vm of
    (prevIndex : restCallStack) -> vm { index = prevIndex, callStack = restCallStack }
    [] -> vm { index = index vm + 1 }

-- Execute JUMP
execute (JUMP label) vm =
  case findLabel (JUMP label) (instructions vm) of
    Just newIndex -> vm { index = newIndex }
    Nothing -> error $ "Label not found: " ++ label

-- Execute JUMP_IF_FALSE
execute (JUMP_IF_FALSE label) vm =
  case stack vm of
    (VBool False : rest) -> case findLabel (JUMP_IF_FALSE label) (instructions vm) of
      Just newIndex -> vm { stack = rest, index = newIndex }
      Nothing -> error $ "Label not found: " ++ label
    (VBool True : rest) -> vm { stack = rest, index = index vm + 1 }
    _ -> error "Stack underflow or invalid type in JUMP_IF_FALSE"

-- Execute LABEL
execute (LABEL _) vm =
  vm { index = index vm + 1 }

-- Execute RETURN
execute RETURN vm =
  case callStack vm of
    (prevIndex:restCallStack) -> vm { index = prevIndex, callStack = restCallStack }
    [] -> error "RETURN called outside of a function"

-- Execute HALT
execute HALT vm =
  vm { index = -1 }

-- | Executes all instructions in the VM until a termination condition is met.
--
-- ==== Parameters
-- * `VM`: The initial state of the virtual machine.
--
-- ==== Returns
-- * `VM`: The final state of the virtual machine after execution.
--
-- ==== Logic
-- 1. La fonction s'arrête si l'index est hors des limites ou si l'instruction `HALT` est rencontrée.
-- 2. Sinon, elle exécute récursivement chaque instruction via `execute`.
--
-- ==== Examples
-- >>> let vm = initializeVM [STORE_CONST (VInt 42), HALT]
-- >>> executeInstructions vm
-- VM {stack = [VInt 42], variables = fromList [], index = -1, indexBeforeFuncCall = Nothing, instructions = [...]}
executeInstructions :: VM -> VM
executeInstructions vm@(VM { index, instructions })
  | index < 0 || index >= length instructions = vm
  | otherwise =
      let instr = instructions !! index
      in executeInstructions (execute instr vm)

-- | Runs a virtual machine with the given list of instructions.
--
-- ==== Parameters
-- * `[Instruction]`: The bytecode instructions to execute.
--
-- ==== Returns
-- * `VM`: The final state of the virtual machine after executing the instructions.
--
-- ==== Examples
-- >>> runVM [STORE_CONST (VInt 42), HALT]
-- VM {stack = [VInt 42], variables = fromList [], index = -1, indexBeforeFuncCall = Nothing, instructions = [...]}
runVM :: [Instruction] -> VM
runVM instructions =
  let vm = initializeVM instructions
  in executeInstructions vm
