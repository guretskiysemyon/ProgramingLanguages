
We have implemented the abstract syntax tree and semantics of the 'While' language in OCaml. Below is our conspectus of the main constructs of the While language, based on the book Semantics with Applications by Flemming Nielson and Hanne Riis Nielson.

# The While Langauge Specification. 

## Syntactic Categories

The various syntactic categories and their corresponding meta-variables are:
1. `n` will range over numerals, **Num**,
2. `x` will range over variables, **Var**,
3. `a` will range over arithmetic expressions, **Aexp**,
4. `b` will range over boolean expressions, **Bexp**,
5. `S` will range over statements, **Stm**.

Note: The structure of numerals and variables is assumed to be predefined.

## Structured Constructs

### Arithmetic Expressions (Aexp)
```
a ::= n | x | a1 + a2 | a1 * a2 | a1 - a2
```

### Boolean Expressions (Bexp)
```
b ::= true | false | a1 = a2 | a1 <= a2 | not b | b1 and b2
```

### Statements (Stm)
```
S ::= x := a | skip | S1; S2 | if b then S1 else S2 | while b do S
```

## Semantic Functions

### Num
To determine the number represented by a numeral:
```
N: Num -> N, N[n] = n
```

### State
A state is a function mapping variables to their current values:
```
State = Var -> Num
```

### Aexp
```
A: Aexp -> (State -> Num)
```

### Bexp
Boolean expressions are functions from states to truth values (`tt` for true, `ff` for false):
```
B[a1 = a2]s = {
    tt if A[a1]s = A[a2]s
    ff otherwise
}
B[a1 <= a2]s = {
    tt if A[a1]s <= A[a2]s
    ff otherwise
}
B[not b]s = {
    tt if B[b]s = ff
    ff otherwise
}
B[b1 and b2]s = {
    tt if B[b1]s = tt and B[b2]s = tt
    ff otherwise
}
```

## Natural Semantics of "While"

### Rules

- **Assignment (ass):**
```
<x := a, s> -> s[x -> A[[n]]a]
```

- **Skip (skip):**
```
<skip, s> -> s
```

- **Composition (comp):**
```
<S1, s> -> s' <S2, s'> -> s'' implies <S1; S2, s> -> s''
```

### Conditional Execution

- If true:
```
<S1, s> -> s' implies <if b then S1 else S2, s> -> s'
```

- If false:
```
<S2, s> -> s' implies <if b then S1 else S2, s> -> s'
```

### Loop Execution

- While loop (if condition is true):
```
<S, s> -> s' <while b do S, s'> -> s'' implies <while b do S, s> -> s''
```

- While loop (if condition is false):
```
<while b do S, s> -> s
```

### The Semantic Function of Statements
```
S_ns: Stm -> (State -> State), S_ns[[S]]s = {
    s' if <S, s> -> s'
    undefined otherwise
}
```


