%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI RARROW LBRACKET RBRACKET BAR
%token PLUS MULT LT EQ COLON COLONCOLON 
%token IF THEN ELSE LET IN FUN REC MATCH WITH
%token INT BOOL LIST

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID EQ Expr SEMISEMI { Decl ($2, $4) }
  | LET ID LPAREN ID COLON Type RPAREN EQ Expr SEMISEMI { Decl ($2, FunExp($4, $6, $9)) }
  | LET REC ID LPAREN ID COLON Type RPAREN COLON Type EQ Expr SEMISEMI { RecDecl ($3, $5, TyFun($7, $10), $12) }

Expr :
    IfExpr { $1 }
  | FunExpr { $1 }
  | LetExpr { $1 }
  | LetRecExpr { $1 }
  | MatchExpr { $1 }
  | LTExpr { $1 }

LTExpr : 
    ConsExpr LT ConsExpr { BinOp (Lt, $1, $3) }
  | ConsExpr { $1 }

ConsExpr :
    PExpr COLONCOLON ConsExpr { BinOp (Cons, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | ID { Var $1 }
  | LBRACKET Type RBRACKET { NilLit $2 }
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

LetExpr :
    LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }
  | LET ID LPAREN ID COLON Type RPAREN EQ Expr IN Expr { LetExp ($2, FunExp($4, $6, $9), $11) }
      
FunExpr :
    FUN LPAREN ID COLON Type RPAREN RARROW Expr { FunExp ($3, $5, $8) }

LetRecExpr :
    LET REC ID LPAREN ID COLON Type RPAREN COLON Type EQ Expr IN Expr { LetRecExp ($3, $5, TyFun($7, $10), $12, $14) }

MatchExpr :
    MATCH Expr WITH 
        LBRACKET RBRACKET RARROW Expr 
    BAR ID COLONCOLON ID RARROW Expr { MatchExp ($2, $7, $9, $11, $13) }

Type :
    LType { $1 }
  | LType RARROW Type { TyFun($1, $3) }

LType :
    AType { $1 }
  | LType LIST { TyList $1 }
        
AType :
    INT { TyInt }
  | BOOL { TyBool }
  | LPAREN Type RPAREN { $2 }        
