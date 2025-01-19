# Kleftis Syntax

## Table of Contents

- [Introduction](#introduction)
- [Data Types](#data-types)
- [Comments](#comments)
- [Syntax Transformations](#syntax-transformations)
- [Functions](#functions)

---

## Introduction

**Kleftis** is inspired by **Python**, **Haskell**, and innovative programming ideas. This document describes the language’s syntax, including types, comments, structures, and functions.

---

## Data Types

Kleftis supports the following data types:

- `ent` – Integer (`int`)
- `reel` – Floating point number (`float`)
- `chaine` – String (`str`)
- `car` – Character (`char`)
- `bool` – Boolean (`True/False`)

---

## Comments

Kleftis provides two types of comments:

- **Multi-line comment**: `<-- text -->`
- **Single-line comment**: `<- text ->`

---

## Syntax Transformations

Kleftis modifies certain standard keywords and symbols:

| Standard | Kleftis Equivalent |
|----------|--------------------|
| `,` | `\|` |
| `"text"` | `#@text#` |
| `return` | `résult` or `result` |
| `if` | `si` |
| `else` | `sinon` |
| `while` | `tantque` |
| `for` | `pour` |

### Example for basic functions

```kleftis
test.nomentier (12)

addition : ent [ent (a b)]
    résult a + b
```

### Example for conditions

```kleftis
recherche : bool [chaine (bob) car (c)]
    si [bob == #@#] (résult Faux)
    si [bob{0} == c] (résult Vrai)
    sinon (résult recherche chaine(bob + 1) c)
```

### Example for loops:

```kleftis
tantque [x > i] (
a = a * a 
x -= 1
)

pour (i : int(2)) [i < 5] (i = i + 1) (a = a + i)
```

---

## Functions

### Declaring a Function with Parameters of the Same Type

```kleftis
nomFonction : typeDeRetour [typeVariable (var1 var2)] (
    ...
)
```

### Declaring a Function with Diffrent Parameter Types

```kleftis
nomFonction : typeDeRetour [ent (var1) reel (var2)] (
    ... 
)
```

### Calling a Function

```kleftis
nomFonction : ent [ent(i)] (
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
