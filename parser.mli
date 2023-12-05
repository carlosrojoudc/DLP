type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | CONCAT
  | BOOL
  | NAT
  | STRING
  | LPAREN
  | RPAREN
  | LCORCH
  | RCORCH
  | COMA
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | INTV of (int)
  | IDV of (string)
  | IDT of (string)
  | STRINGV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.term
