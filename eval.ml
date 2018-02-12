open Syntax 

type exval = 
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
  | NilV
  | ConsV of exval * exval
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec pp_val = function
    IntV i -> 
      print_int i
  | BoolV b -> 
      if b then print_string "true" else print_string "false"
  | ProcV (_, _, _) ->
      print_string "<fun>"
  | NilV -> print_string "[]"
  | ConsV (v1, v2) -> 
      (match v1 with ConsV (_,_) -> print_string "(" | _ -> ());
      pp_val v1; 
      (match v1 with ConsV (_,_) -> print_string ")" | _ -> ());
      print_string " :: "; pp_val v2

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | Cons, v1, v2 -> ConsV (v1, v2)

let rec eval_exp env = function
    Var x -> Environment.lookup x env
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
	(match test with
	    BoolV b -> if b then eval_exp env exp2 else eval_exp env exp3
	  | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
      let value = eval_exp env exp1 in
	eval_exp (Environment.extend id value env) exp2
  | FunExp (id, _, exp) -> ProcV (id, exp, ref env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
	(match funval with
	     ProcV (id, body, env') -> 
	       let newenv = Environment.extend id arg !env' in
		 eval_exp newenv body 
	   | _ -> err ("Non-function value is applied"))
  | LetRecExp (id, para, _, exp1, exp2) ->
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
	dummyenv := newenv;
	eval_exp newenv exp2
  | NilLit _ -> NilV
  | MatchExp (test, nil_case, car, cdr, cons_case) ->
      match eval_exp env test with
	  NilV -> eval_exp env nil_case
	| ConsV (v1, v2) ->
	    let newenv = Environment.extend car v1 (Environment.extend cdr v2 env) in
	      eval_exp newenv cons_case
	| _ -> err "Non-list value is tested with match"

let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) -> 
      let v = eval_exp env e in (id, Environment.extend id v env, v)
  | RecDecl (id, para, _, body) -> 
      let dummyenv = ref Environment.empty in
      let raw_closure = ProcV (para, body, dummyenv) in
      let newenv = Environment.extend id raw_closure env in
	dummyenv := newenv;
	(id, newenv, raw_closure)
