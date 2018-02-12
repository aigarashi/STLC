# Interpreter of STLC

## Build instructions

* Do `make depend` and `make`.  You'll get `miniml`.
    * Or, do `omake`.

## Syntax

* Types: `int`, `bool`, `S->T`.
* Constants: integers, `true`, `false`
* Usual constructs such as: `let x = e1 in e2`, `if e1 then e2 else e3`, `+`, `*`, `<`
* Abstraction: `fun (x : T) -> e`
* Recursion: `let rec f (x:S) : T = e in e`
    * The return type annotation `: T` is mandatory.
* Lists: `[T]` (empty list of `T list`), `e::e` (cons)
* Case analysis on lists: `match e with [] -> e | x :: y -> e`
* Top-level input: `e;;`, `let x : T = e;;`, `let f (x:T1) = e;;`, or `let rec f (x:T1) : T = e;;`.

