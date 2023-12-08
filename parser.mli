type token =
  | SPACE
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
  | CAPITALIZE
  | BOOL
  | NAT
  | STRING
  | LIST
  | FIX
  | LPAREN
  | RPAREN
  | LCORCH
  | RCORCH
  | LBRACK
  | RBRACK
  | AS
  | CASE
  | OF
  | BIGARROW
  | LTAG
  | RTAG
  | NILLIST
  | CONSLIST
  | ISNILLIST
  | HEADLIST
  | TAILLIST
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
