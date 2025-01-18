# Kleftis Syntax

## Table of Contents

- [Introduction](#introduction)
- [Data Types](#data-types)
- [Comments](#comments)
- [Syntax Transformations](#syntax-transformations)
- [Structures](#structures)
- [Functions](#functions)
- [Some Examples](#some-examples)

---

## Introduction

**Kleftis** is inspired by **Python**, **Haskell**, and innovative programming ideas. This document describes the language’s syntax, including types, comments, structures, and functions.

---

## Data Types

Kleftis supports the following data types:

- `ent` – Integer (`int`)
- `réel` – Floating point number (`float`)
- `chaine` – String (`str`)
- `car` – Character (`char`)
- `bool` – Boolean (`true/false`)

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
| `,` | `|` |
| `"text"` | `#@text#` |
| `return` | `résult` or `result` (Could be replaced with `renvoyer` if needed) |
| `if` | `Si` |
| `else` | `Sinon` |
| `while` | `PendantQue` or `Pendant que` |

### Example for `PendantQue`:

```kleftis
PendantQue x < 10
    x = x + 1
```

---

## Structures

### Declaring a Structure

```kleftis
structure nomStructure (
    nomVariableEnt: ent
   nomVariableCar: car
   nomList: chaine{}
)
```

### Initializing a Structure

```kleftis
nomStructure.nomVariableEnt (12)
nomStructure.nomVariableCar (#p)
nomStructure.nomList.ajout {#@hello world#}
```

---

## Functions

### Declaring a Function with Parameters of the Same Type

```kleftis
nomFonction : typeDeRetour [typeVariable (var1 var2)] 
```

### Declaring a Function with Diffrent Parameter Types

```kleftis
nomFonction : typeDeRetour [ent (var1) réel (var2)]
```

### Calling a Function

```kleftis
résultat : typeDeRetour (nomFonction param1 param2)
```

---

## Some Examples

```kleftis
carré : ent [ent (a)] résult a * a

structure test (
    nomentier: ent
    nomcar: car(#p)
    list:chaine{#@coucou# | #@ prout#}
)

test.nomentier (12)
test.list.ajout{#@ prout encore#}

addition : ent [ent (a b)]
    résult a + b

addition : réel [réel (a b)]
    résult a + b

carré : ent [ent (z)]
    résult z * z

max : ent [ent (a) ent (b)]
    si a > b
        résult a
    sinon
        résult b

recherche : bool [chaine (bob) car (c)]
    si bob == #@#
        résult Faux
    si bob{0} == c
        résult Vrai
    sinon
        résult recherche chaine(bob + 1) c

résultat : ent (addition 3 5) + test.nomentier
```