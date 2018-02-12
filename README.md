# Interpreter of STLC

## Build instructions

* Do `make depend` and `make`.  You'll get `miniml`.
    * Or, do `omake`.

## Syntax

It's like OCaml but type annotations are required for variable declarations.

* Types: `int`, `bool`, `S->T`.
* Constants: integers, `true`, `false`
* Usual constructs such as: `let x = e1 in e2`, `if e1 then e2 else e3`, `+`, `*`, `<`
* Abstraction: `fun (x : T) -> e`
    * The argument type `: T` is mandatory.
* Recursion: `let rec f (x:S) : T = e in e`
    * The return type annotation `: T` is mandatory.
* Lists: `[T]` (empty list of `T list`), `e::e` (cons)
* Case analysis on lists: `match e with [] -> e | x :: y -> e`
* Top-level input: `e;;`, `let x : T = e;;`, `let f (x:T1) = e;;`, or `let rec f (x:T1) : T = e;;`.

## Extending to GTLC

* Check out the branch `gtlc`.
   * `Syntax.ty` has been extended.
   * `Syntax.IL` is the module for syntactic entities in the IL.
       * `Syntax.IL.tag` defines type tags G.
       * `Syntax.IL.exp` defines expressions in the IL.
* Implement (or implement) the following types and functions:
   * `Typing.con : Syntax.ty -> Syntax.ty -> bool` to judge type consistency.
   * `Typing.translate : Typing.tyenv -> Syntax.exp -> (Syntax.ty * Syntax.IL.exp)` to typecheck the given expression and translate to an IL expression.
   * `Eval.exval` so that it contains tagged values.
   * `Eval.cast : Eval.exval -> Syntax.ty -> Syntax.ty -> Eval.exval` to evaluate a cast from one type to another.
   * `Eval.eval : Eval.exval Environment.t -> Syntax.IL.exp -> Eval.exval`.
