# Kleftis Parser Modules

This repository contains three Haskell modules that handle parsing for the [Kleftis language](./Syntaxe.md). These modules utilize the Megaparsec library to process symbolic expressions (SExpr) and translate them into an abstract syntax tree (AST). Below is a detailed breakdown of each module and its functionality.

---

## 1. ParserCompilVM.hs

### Description

This module provides a parser for processing strings into structured instructions. It handles Kleftis syntax parsing, including integer parsing, string handling, boolean values, and custom instructions.

### Key Functions

- `noSpaceInst`: Skips spaces and tabs while handling comments.
- `parsIntInst`: Parses signed integers.
- `parsFloatInst`: Parses signed floating-point numbers.
- `parseStringInst`: Parses quoted strings.
- `parseBoolInst`: Parses boolean values (`True`, `False`).
- `parseOperator`: Parses binary operators (`ADD`, `SUBTRACT`, `MULTIPLY`, etc.).
- `parseComparator`: Parses binary comparison operators (`COMPARE_GT`, `COMPARE_LT`, etc.).
- `parseInstruction`: Parses individual instructions like `STORE_CONST`, `LOAD_VAR`, `JUMP`, `CALL`, and `HALT`.
- `pGroupedExprInst`: Parses grouped expressions separated by newlines.
- `pProgramInst`: Parses a complete program into a list of instructions.

---

## 2. ParserKleftisSExp.hs

### Description

This module is responsible for parsing symbolic expressions (SExpr) in the Kleftis language. It defines functions to process atomic values, function definitions, control structures (`if`, `for`, `while`), and structured expressions.

### Key Functions

- `noSpace`: Skips spaces and comments.
- `parseAtom`: Parses symbols and variables.
- `parseString`: Parses custom string literals (`#@ ... #`).
- `parseType`: Parses type annotations (`int`, `float`, `bool`, etc.).
- `parseBool`: Parses boolean values.
- `parseBasicFunc`: Parses basic functions (`+`, `-`, `*`, `/`).
- `pExpr`: Parses individual expressions like numbers, strings, lists, and conditionals.
- `pGroupedExpr`: Parses grouped expressions separated by double newlines.
- `pProgram`: Parses an entire S-expression program.

---

## 3. ParserSExpAST.hs

### Description

This module translates parsed symbolic expressions (SExpr) into an Abstract Syntax Tree (AST). It processes function definitions, conditionals, loops, and function calls.

### Key Functions

- `findFuncOrDef`: Identifies function or variable definitions and converts them into AST nodes.
- `findIf`: Processes conditional expressions (`if ... else`).
- `findLoop`: Parses `while` loops.
- `findFor`: Parses `for` loops, including initialization, condition, and increment expressions.
- `findCall`: Parses function calls, extracting the function name and parameters.
- `parseFinalAST`: Converts SExpr into AST structures.

---

## Usage

These modules are designed to be used together to parse and process Kleftis programs. 

Example workflow:

1. Use `ParserKleftisSExp.hs` to parse raw input into symbolic expressions.
2. Convert symbolic expressions into AST nodes using `ParserSExpAST.hs`.
3. Process the parsed AST with `ParserCompilVM.hs` for instruction-level parsing.

---

## Dependencies

- [Megaparsec](https://hackage.haskell.org/package/megaparsec)
- Standard Haskell libraries (`Data.Void`, `Control.Monad`)
