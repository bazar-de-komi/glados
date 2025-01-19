# Kleftis Programming Language

Welcome to Kleftis, a minimalistic programming language inspired by Lisp. This document introduces the syntax of Kleftis and provides examples to help you get started. The Kleftis language is parsed into **S-Expressions (SExpr)** and supports basic programming constructs such as variables, functions, conditionals, loops, and more.

---

## Syntax Overview

Kleftis uses symbolic expressions (SExpr) for all its constructs. Each construct is enclosed in specific delimiters such as `()`, `[]`, or `{}`. Below is a quick guide to the syntax.

---

### 1. Variables and Definitions

You can define variables using the `define` keyword:

```kleftis
define x 42           ; Define an integer
define y 3.14         ; Define a float
define name #@John#   ; Define a string
define list {1 2 3}   ; Define a list
```

---

### 2. Functions, Conditionals, Loops, and Data Types

#### Functions

Functions are defined by using their name followed by `:`, with their type of return, their parameters enclosed in `[]` and the body enclosed in `()`:

```kleftis
nomFonction : ent [ent (i)] (
    (a:ent(0))
    tantque[i>0] (
        (i=i- 1)
        (a = a+1)
    )
    pour (i:ent(0)) [i<3] (i = i + 1) (
        (a = a * a)
    )
    result a
)
```

#### Conditionals

Kleftis supports conditionals using `si` (if) and `sinon` (else):

```kleftis
si [x > y]
  (result x)
sinon
  (result y)
```

The condition is enclosed in `[]`, the "then" branch in `()`, and the "else" branch in `()` after `sinon`.

#### Loops

**While Loop (`tantque`)**

The `tantque` loop executes as long as the condition is true:

```kleftis
tantque [x < 10]
  (result x)
```

**For Loop (`pour`)**

The `pour` loop includes initialization, condition, update, and body:

```kleftis
pour (= i 0) [i < 10] (i += 1)
  (result i)
```

#### Data Types

Kleftis supports the following data types:

- **Integer**: `ent`
- **Float**: `reel`
- **String**: Enclosed in `#@ ... #`
- **Boolean**: `True`, `False`
- **Character**: `#a`

Examples:

```kleftis
define flag True       ; Boolean
define char #a         ; Character
define num ent 42      ; Integer
define pi reel 3.14    ; Float
define text chaine #@Hello, Kleftis!# ; String
```

---

### 3. Lists and Operations

#### Lists

Kleftis supports lists enclosed in `{}`:

```kleftis
define nums {1 2 3 4 5}
define words {#@hello# #@world#}
```

#### Operations

Kleftis supports basic arithmetic and comparison operations:

- **Arithmetic**: `+`, `-`, `*`, `/`
- **Comparison**: `>`, `<`, `>=`, `<=`, `==`, `!=`

---

### 4. Return Expressions and Comments

#### Return Expressions

The `result` or `résult` keyword is used to return a value from a block or function:

```kleftis
result a + b
résult a * b
```

#### Comments

Kleftis supports two types of comments:

- **Single-line comments**: `<-`

  ```kleftis
  <- This is a single-line comment
  ```

- **Block comments**: `<-- ... -->`

  ```kleftis
  <--
  This is a
  multi-line comment
  -->
  ```

---

## Example Programs

### Example 1: Basic Operations

```kleftis
define x 42
define y 3.14
si [x > y]
  (result x)
sinon
  (result y)
```

### Example 2: Loops

```kleftis
pour (= i 0) [i < 5] (i += 1)
  (result i)

tantque [x < 10]
  (result x)
```

### Example 3: Functions

```kleftis
define add [a b]
  (result a + b)

define factorial [n]
  si [n <= 1]
    (result 1)
  sinon
    (result n * factorial [n - 1])
```

---

## Getting Started

- Write your Kleftis code in a `.kleftis` file.
- Use the provided parser module (`Parser.ParserKleftisSExp`) to parse the program and convert it into an abstract syntax tree (AST).
- Execute your program using the interpreter or virtual machine provided with the project.

---

Enjoy coding in Kleftis! 🚀
