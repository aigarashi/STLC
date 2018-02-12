(* ML interpreter / type reconstruction *)
type id = string

type ty = 
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyList of ty
  | TyDyn

type binOp = Plus | Mult | Lt | Cons

type exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * ty * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * ty * exp * exp
  | NilLit of ty
  | MatchExp of exp * exp * id * id * exp

type program = 
  | Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * ty * exp

(* pretty printing *)
let index2name i = (* i must be non negative *)
  let rec aux i = 
    if i < 26 then [char_of_int (i + 97)]
    else char_of_int (i mod 26 + 97) :: aux (i / 26) in
  let b = Buffer.create 16 in
    Buffer.add_char b '\'';
    List.iter (fun c -> Buffer.add_char b c) (aux i);
    Buffer.contents b

module IL =
  struct
    type tag = Int | Bool | Fun (* ?->? *) | List (* ? list *)

    type exp =
      | Var of id
      | ILit of int
      | BLit of bool
      | BinOp of binOp * exp * exp
      | IfExp of exp * exp * exp
      | LetExp of id * exp * exp
      | FunExp of id * ty * exp
      | AppExp of exp * exp
      | LetRecExp of id * id * ty * exp * exp
      | NilLit of ty
      | MatchExp of exp * exp * id * id * exp
      | CastExp of exp * ty * ty
  end

let pp_ty ty = 
  let rec aux = function
    | TyInt -> print_string "int"
    | TyBool -> print_string "bool"
    | TyFun (domty, ranty) -> 
	(match domty with TyFun (_,_) -> print_string "(" | _ -> ());
	aux domty; 
	(match domty with TyFun (_,_) -> print_string ")" | _ -> ());
	print_string " -> "; aux ranty
    | TyList elmty ->
	(match elmty with TyFun (_,_) -> print_string "(" | _ -> ());
	aux elmty; 
	(match elmty with TyFun (_,_) -> print_string ")" | _ -> ());
	print_string " list"
  in aux ty

let pp_rawty ty = 
  let rec aux = function
    | TyInt -> print_string "int"
    | TyBool -> print_string "bool"
    | TyFun (domty, ranty) -> 
	(match domty with TyFun (_,_) -> print_string "(" | _ -> ());
	aux domty; 
	(match domty with TyFun (_,_) -> print_string ")" | _ -> ());
	print_string " -> "; aux ranty
    | TyList elmty ->
	(match elmty with TyFun (_,_) -> print_string "(" | _ -> ());
	aux elmty; 
	(match elmty with TyFun (_,_) -> print_string ")" | _ -> ());
	print_string " list"
  in aux ty
