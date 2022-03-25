type token =
  | ATOM of (string)
  | TRUE
  | FALSE
  | NOT
  | AND
  | OR
  | IMP
  | IFF
  | RPAREN
  | LPAREN
  | EOF

val formula :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Language.form
