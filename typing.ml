open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with 
		 TyInt, TyInt -> TyInt
	       | _ -> err "Argument must be of integer: +")
  | Mult -> (match ty1, ty2 with 
		 TyInt, TyInt -> TyInt
	       | _ -> err "Argument must be of integer: *")
  | Lt -> (match ty1, ty2 with 
		 TyInt, TyInt -> TyBool
	       | _ -> err "Argument must be of integer: <")
  | Cons -> (match ty2 with
               TyList ty20 -> if ty20 = ty1 then ty2 else err "Type mismatch: ::"
             | _ -> err "Second argument must be of list: ::")

let rec ty_exp tyenv = function
    Var x -> 
     (try Environment.lookup x tyenv with
        Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let ty1 = ty_exp tyenv exp1 in
      let ty2 = ty_exp tyenv exp2 in
      ty_prim op ty1 ty2
  | IfExp (exp1, exp2, exp3) ->
      let tytest = ty_exp tyenv exp1 in
      let tythen = ty_exp tyenv exp2 in
      let tyelse = ty_exp tyenv exp3 in
	if tytest = TyBool then
	  if tythen = tyelse then tythen
	  else err "Types of branches must agree: if"
	else err "Test expression must be of bool: if"
  | LetExp (id, exp1, exp2) ->
     let ty1 = ty_exp tyenv exp1 in
     ty_exp (Environment.extend id ty1 tyenv) exp2
  | FunExp (id, ty, exp) ->
     TyFun(ty, ty_exp (Environment.extend id ty tyenv) exp)
  | AppExp (exp1, exp2) ->
      let ty1 = ty_exp tyenv exp1 in
      let ty2 = ty_exp tyenv exp2 in
      (match ty1 with
         TyFun(ty11, ty12) -> if ty11 = ty2 then ty12
                              else err "The argument type doesn't match the formal"
       | _ -> err "A function is expected")
  | LetRecExp (id, para, (TyFun(tyfun1, tyfun2) as tyfun), exp1, exp2) ->
     let tyenv' = 
       Environment.extend id tyfun
	 (Environment.extend para tyfun1 tyenv) in
     let ty1 = ty_exp tyenv' exp1 in
     if ty1 = tyfun2 then
       ty_exp (Environment.extend id tyfun tyenv) exp2
     else err "The declared return type doesn't match the type of the body: let rec"
  | NilLit ty -> TyList ty
  | MatchExp (test, nil_case, hd, tl, cons_case) ->
     let ty1 = ty_exp tyenv test in
     (match ty1 with
        TyList ty10 ->
        let ty_nilcase = ty_exp tyenv nil_case in
        let ty_conscase = ty_exp (Environment.extend hd ty10 (Environment.extend tl ty1 tyenv)) cons_case in
        if ty_nilcase = ty_conscase then ty_nilcase
        else err "The types of two braches do not match: match"
      | _ -> err "A list is expected: match")

let ty_decl tyenv = function
    Exp e -> (tyenv, ty_exp tyenv e)
  | Decl (id, e) -> 
     let ty = ty_exp tyenv e in
     (Environment.extend id ty tyenv, ty)
  | RecDecl (id, para, (TyFun(tyfun1, tyfun2) as tyfun), exp) -> 
     let tyenv' = 
       Environment.extend id tyfun
	 (Environment.extend para tyfun1 tyenv) in
     let ty1 = ty_exp tyenv' exp in
     if ty1 = tyfun2 then
       (Environment.extend id tyfun tyenv, tyfun)
     else err "The declared return type doesn't match the type of the body: let rec"


