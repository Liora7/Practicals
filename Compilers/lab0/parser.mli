type token =
  | IDENT of (string)
  | NUMBER of (float)
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | OPEN
  | CLOSE
  | EQUAL
  | EOF
  | BADTOK

val equation :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string * Tree.expr
