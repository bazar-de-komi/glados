
# Virtual Machine

## Table of Contents

- [Introduction](#introduction)
- [Initialization](#initialization)
- [Finding Labels](#finding-labels)
- [Executing Instructions](#executing-instructions)
- [Instruction Types](#instruction-types)
- [Executing the VM](#executing-the-vm)
- [Examples](#examples)

---

## Introduction

This module provides the implementation of a **Virtual Machine (VM)** capable of executing a custom [bytecode](../Compiler/Bytecode.md) language.  

It includes functions for:  

- **Initializing the VM**
- **Handling execution of various instructions**
- **Managing control flow**
- **Variable storage**
- **Arithmetic and logical operations**
- **Function calls**
- **Program termination**

---

## Initialization

### **Function: `initializeVM`**

Initializes a virtual machine (VM) with a given list of instructions.

#### **Parameters**:

- `[Instruction]`: A list of bytecode instructions that the VM will execute.

#### **Returns**:

- `VM`: A virtual machine instance with:
  - An **empty stack**
  - An **empty variable map**
  - The **program counter (`index`) set to 0**.

#### **Example**

```haskell
>>> initializeVM [STORE_CONST (VInt 42), HALT]
VM {stack = [], variables = fromList [], index = 0, indexBeforeFuncCall = Nothing, instructions = [STORE_CONST (VInt 42), HALT]}
```

## Finding Labels

### **Function**: `[findLabel]`

Finds the index of a label in a list of instructions.

#### **Parameters**:

- `String`: The label to search for.
- `[Instruction]`: The list of instructions to search within.

#### **Returns**:

- `Maybe Int`: The index of the instruction containing the label, or `Nothing` if not found.

#### Examples

```haskell
>>> findLabel "start" [LABEL "start", STORE_CONST (VInt 42)]
Just 0

>>> findLabel "end" [STORE_CONST (VInt 42), LABEL "end"]
Just 1

>>> findLabel "missing" [STORE_CONST (VInt 42)]
Nothing
```

---

## Executing Instructions

### **Function**: `execute`

Executes a single instruction on the virtual machine.

#### **Parameters**:

- `Instruction`: The instruction to execute.
- `VM`: The current state of the virtual machine.

#### **Returns**:

- `VM`: The updated state of the virtual machine after executing the instruction.

#### **Logic Breakdown**

This function handles different instruction types:

1. `STORE_CONST`: Pushes a constant value onto the stack.
2. `STORE_VAR`: Stores a value from the stack into a variable.
3. `LOAD_VAR`: Loads a variable onto the stack.
4. `OPERATOR`: Executes arithmetic operations (+, -, *, /, %).
5. `COMPARATOR`: Performs comparisons (>, <, ==, !=, etc.).
6. `CALL`: Calls a function and updates the program counter.
7. `LABEL_FUNC` / `LABEL_FUNC_END`: Manages function labels.
8. `JUMP` / `JUMP_IF_FALSE`: Handles conditional and unconditional jumps.
9. `RETURN`: Returns from a function call.
10. `HALT`: Stops the virtual machine.

#### **Example**

```haskell
>>> let vm = initializeVM [STORE_CONST (VInt 42)]
>>> execute (STORE_CONST (VInt 42)) vm
VM {stack = [VInt 42], variables = fromList [], index = 1, indexBeforeFuncCall = Nothing, instructions = [...]}
```

## Instruction Types

### **Arithmetic Operations** (`OPERATOR`)

- Takes `two values from the stack` and `stores the result` back onto the stack.

| Operator    | Description        |
| ------------|--------------------|
| `ADD`       | Addition           |
| `SUBTRACT`  | Subtraction        |
| `MULTIPLY`  | Multiplication     |
| `DIVIDE`    | Division           |
| `MODULO`    | Modulus (remainder)|

### **Comparisons** (`COMPARATOR`)

- Takes `two values from the stack` and pushes `true` or `false` based on the comparison.

| Comparator   | Meaning                     |
| -------------|---------------------------- |
| `COMPARE_GT` | Greater Than (`>`)          |
| `COMPARE_LT` | Lesser Than (`<`)           |
| `COMPARE_EQ` | Equal (`==`)                |
| `COMPARE_NE` | Not Equal (`!=`)            |
| `COMPARE_GE` | Greater than or Equal (`>=`)|
| `COMPARE_LE` | Lesser than or Equal (`<=`) |

### **Control Flow Instructions**

| Instruction             | Description                                                 |
| ------------------------|------------------------------------------------------------ |
| `JUMP <label>`          | Jumps to the instruction marked by `<label>`.               |
| `JUMP_IF_FALSE <label>` | Jumps to `<label>` if the top value of the stack is `false`.|
| `LABEL <name>`          | Marks a position in the program for jumps.                  |

### **Function Handling**

| Instruction             | Description                                                          |
| ------------------------|--------------------------------------------------------------------- |
| `LABEL_FUNC <name>`     | Marks the start of a function.                                       |
| `LABEL_FUNC_END <name>` | Marks the end of a function.                                         |
| `CALL <name>`           | Calls a function. Arguments are pushed onto the stack before calling.|
| `RETURN`                | Ends a function and returns execution to the caller.                 |

### **Program Termination**

| Instruction   | Description                            |
| --------------|--------------------------------------- |
| `HALT`        | Stops execution of the virtual machine.|

## Executing the VM

### **Function**: `executeInstructions`

Executes all instructions in the VM until termination.

#### Parameters:

- `VM`: The initial state of the virtual machine.

#### Returns:

- `VM`: The final state of the virtual machine after execution.

#### Logic

1. The function stops if the index is out of bounds or if HALT is encountered.
2. Otherwise, it recursively executes each instruction.

#### Example

```haskell
>>> let vm = initializeVM [STORE_CONST (VInt 42), HALT]
>>> executeInstructions vm
VM {stack = [VInt 42], variables = fromList [], index = -1, indexBeforeFuncCall = Nothing, instructions = [...]}
```

## Running the Virtual Machine

### **Function**: `runVM`

Executes a program given a list of instructions.

#### **Parameters**:

- `[Instruction]`: The bytecode instructions to execute.

#### **Returns**:

- `VM`: The final state of the VM.

#### **Example**

```haskell
>>> runVM [STORE_CONST (VInt 42), HALT]
VM {stack = [VInt 42], variables = fromList [], index = -1, indexBeforeFuncCall = Nothing, instructions = [...]}
```

## Examples

### **Basic Arithmetic**

```haskell
runVM [STORE_CONST (VInt 5), STORE_CONST (VInt 3), OPERATOR ADD, HALT]
```

**Stack after execution**: `[VInt 8]`

### **Conditional Jump**

```haskell
runVM [STORE_CONST (VBool False), JUMP_IF_FALSE "end", STORE_CONST (VInt 42), LABEL "end", HALT]
```

- Since the **top of the stack** is `False`, execution **jumps** to `LABEL "end"`.

### **Function Call**

```haskell
runVM [LABEL_FUNC "double", STORE_CONST (VInt 2), OPERATOR MULTIPLY, RETURN, CALL "double", STORE_CONST (VInt 5), HALT]
```

- Calls `double`, multiplying the value by 2 before returning.
