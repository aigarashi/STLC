{
let reservedWords = [
  (* Keywords *)
  ("bool", Parser.BOOL);
  ("else", Parser.ELSE);
  ("fun", Parser.FUN);
  ("if", Parser.IF);
  ("in", Parser.IN);
  ("int", Parser.INT);
  ("let", Parser.LET);
  ("list", Parser.LIST);
  ("match", Parser.MATCH);
  ("rec", Parser.REC);
  ("then", Parser.THEN);
  ("with", Parser.WITH);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "=" { Parser.EQ }
| "->" { Parser.RARROW }
| "[" { Parser.LBRACKET }
| "]" { Parser.RBRACKET }
| "|" { Parser.BAR }
| "::" { Parser.COLONCOLON }
| ":" { Parser.COLON }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }


