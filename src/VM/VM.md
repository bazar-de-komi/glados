# Kleftis Virtual Machine (VM)

This module implements a virtual machine (VM) capable of executing a custom bytecode language. It handles instruction execution, control flow, variable storage, arithmetic and logical operations, function calls, and program termination.

---

## **VM Functionality**

### **1. Initialization**
- `initializeVM`: Creates a VM instance with an empty stack, variable storage, and a program counter set to 0.

### **2. Execution**
- `execute`: Processes a single instruction, updating the VM state accordingly.
- `executeInstructions`: Runs all instructions until the program halts.
- `runVM`: Initializes and executes the VM with a given list of instructions.

### **3. Instruction Handling**
- **Data Operations**: `STORE_CONST`, `STORE_VAR`, `LOAD_VAR`
- **Arithmetic & Logic**: `OPERATOR` (addition, subtraction, etc.), `COMPARATOR` (>, <, ==, etc.)
- **Control Flow**: `JUMP`, `JUMP_IF_FALSE`, `HALT`
- **Functions**: `CALL`, `RETURN`, `LABEL_FUNC`, `LABEL_FUNC_END`

### **4. Label Resolution**
- `findLabel`: Searches for labels in the instruction set to handle jumps and function calls.

---

## **Example**

```haskell
runVM [STORE_CONST (VInt 42), HALT]
```
**Output:**
```
VM {stack = [VInt 42], variables = fromList [], index = -1, ...}
```

---

## **Dependencies**
- Standard Haskell libraries (`Data.Map`, `Data.List`)
