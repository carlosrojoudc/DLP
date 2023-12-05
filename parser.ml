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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 38 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* LETREC *);
  268 (* IN *);
  269 (* CONCAT *);
  270 (* BOOL *);
  271 (* NAT *);
  272 (* STRING *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* LCORCH *);
  276 (* RCORCH *);
  277 (* COMA *);
  278 (* DOT *);
  279 (* EQ *);
  280 (* COLON *);
  281 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  282 (* INTV *);
  283 (* IDV *);
  284 (* IDT *);
  285 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\005\000\005\000\005\000\005\000\006\000\
\006\000\007\000\007\000\003\000\003\000\003\000\003\000\003\000\
\003\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\004\000\004\000\009\000\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\002\000\001\000\006\000\006\000\006\000\008\000\003\000\003\000\
\003\000\003\000\003\000\004\000\003\000\001\000\000\000\005\000\
\000\000\003\000\001\000\001\000\002\000\002\000\002\000\003\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\027\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
\000\000\032\000\040\000\000\000\000\000\020\000\000\000\000\000\
\029\000\030\000\021\000\022\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\025\000\000\000\000\000\000\000\000\000\024\000\026\000\000\000\
\000\000\009\000\000\000\036\000\037\000\038\000\000\000\039\000\
\011\000\000\000\007\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\035\000\034\000\000\000\000\000\
\000\000\000\000\000\000\018\000\000\000\000\000\000\000\000\000\
\016\000"

let yydgoto = "\002\000\
\019\000\066\000\021\000\057\000\036\000\075\000\067\000\022\000\
\058\000"

let yysindex = "\011\000\
\093\255\000\000\232\254\000\000\000\000\093\255\011\255\011\255\
\011\255\009\255\014\255\011\255\093\255\122\255\000\000\020\255\
\031\255\000\000\000\000\002\000\011\255\000\000\038\255\255\254\
\000\000\000\000\000\000\000\000\000\000\040\255\041\255\011\255\
\028\255\044\255\244\254\048\255\093\255\070\255\253\254\000\000\
\000\000\070\255\093\255\093\255\070\255\000\000\000\000\093\255\
\093\255\000\000\039\255\000\000\000\000\000\000\070\255\000\000\
\000\000\045\255\000\000\000\000\042\255\250\254\249\254\049\255\
\030\255\037\255\000\000\055\255\070\255\093\255\093\255\093\255\
\093\255\047\255\000\000\093\255\000\000\000\000\039\255\039\255\
\039\255\023\255\056\255\000\000\093\255\093\255\039\255\030\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\061\255\000\000\001\000\
\029\000\000\000\000\000\000\000\066\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\255\062\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\071\000\000\000\000\000\000\000\000\000\000\000\
\000\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\063\255\069\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\088\000\092\000\
\109\000\000\000\000\000\000\000\000\000\000\000\113\000\073\255\
\000\000"

let yygindex = "\000\000\
\000\000\005\000\000\000\231\255\000\000\017\000\023\000\048\000\
\000\000"

let yytablesize = 390
let yytable = "\071\000\
\029\000\040\000\023\000\043\000\072\000\020\000\029\000\029\000\
\049\000\039\000\024\000\001\000\004\000\005\000\039\000\039\000\
\061\000\033\000\035\000\064\000\039\000\029\000\059\000\060\000\
\029\000\029\000\029\000\013\000\030\000\068\000\029\000\029\000\
\029\000\029\000\085\000\030\000\015\000\025\000\026\000\018\000\
\031\000\051\000\037\000\078\000\039\000\047\000\033\000\062\000\
\063\000\039\000\074\000\039\000\065\000\038\000\027\000\028\000\
\029\000\076\000\039\000\032\000\039\000\042\000\044\000\070\000\
\045\000\002\000\048\000\050\000\041\000\069\000\010\000\073\000\
\077\000\083\000\079\000\080\000\081\000\082\000\086\000\046\000\
\015\000\014\000\010\000\052\000\053\000\054\000\055\000\004\000\
\019\000\087\000\088\000\003\000\017\000\003\000\004\000\005\000\
\006\000\056\000\084\000\007\000\008\000\009\000\010\000\011\000\
\089\000\012\000\000\000\000\000\005\000\013\000\000\000\014\000\
\006\000\000\000\000\000\000\000\000\000\000\000\015\000\016\000\
\017\000\018\000\003\000\004\000\005\000\006\000\000\000\000\000\
\007\000\008\000\009\000\010\000\011\000\000\000\012\000\000\000\
\000\000\000\000\013\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\034\000\017\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\029\000\029\000\000\000\029\000\029\000\000\000\
\000\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
\000\000\029\000\029\000\000\000\029\000\029\000\029\000\039\000\
\000\000\000\000\029\000\029\000\029\000\029\000\030\000\030\000\
\000\000\030\000\030\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\030\000\030\000\000\000\
\030\000\030\000\030\000\033\000\033\000\000\000\030\000\030\000\
\030\000\030\000\033\000\000\000\000\000\000\000\000\000\000\000\
\033\000\000\000\033\000\033\000\033\000\033\000\002\000\002\000\
\000\000\000\000\000\000\010\000\010\000\002\000\000\000\000\000\
\000\000\000\000\010\000\002\000\000\000\002\000\002\000\002\000\
\010\000\000\000\010\000\010\000\004\000\004\000\000\000\000\000\
\003\000\003\000\000\000\004\000\000\000\000\000\000\000\003\000\
\000\000\004\000\000\000\004\000\004\000\003\000\000\000\003\000\
\003\000\005\000\005\000\000\000\000\000\006\000\006\000\000\000\
\005\000\000\000\000\000\000\000\006\000\000\000\005\000\000\000\
\005\000\005\000\006\000\000\000\006\000\006\000"

let yycheck = "\006\001\
\000\000\000\000\027\001\005\001\012\001\001\000\002\001\003\001\
\021\001\022\001\006\000\001\000\002\001\003\001\022\001\022\001\
\042\000\013\000\014\000\045\000\022\001\017\001\026\001\027\001\
\020\001\021\001\022\001\017\001\000\000\055\000\026\001\027\001\
\028\001\029\001\012\001\027\001\026\001\027\001\028\001\029\001\
\027\001\037\000\023\001\069\000\022\001\018\001\000\000\043\000\
\044\000\022\001\021\001\022\001\048\000\023\001\007\000\008\000\
\009\000\021\001\022\001\012\000\022\001\024\001\023\001\022\001\
\024\001\000\000\023\001\020\001\021\000\025\001\000\000\023\001\
\018\001\027\001\070\000\071\000\072\000\073\000\023\001\032\000\
\020\001\020\001\020\001\014\001\015\001\016\001\017\001\000\000\
\020\001\085\000\086\000\000\000\020\001\001\001\002\001\003\001\
\004\001\028\001\076\000\007\001\008\001\009\001\010\001\011\001\
\088\000\013\001\255\255\255\255\000\000\017\001\255\255\019\001\
\000\000\255\255\255\255\255\255\255\255\255\255\026\001\027\001\
\028\001\029\001\001\001\002\001\003\001\004\001\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\255\255\
\255\255\255\255\017\001\255\255\019\001\255\255\255\255\255\255\
\255\255\255\255\255\255\026\001\027\001\028\001\029\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\255\255\
\255\255\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\017\001\018\001\255\255\020\001\021\001\022\001\022\001\
\255\255\255\255\026\001\027\001\028\001\029\001\002\001\003\001\
\255\255\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\255\255\255\255\255\255\255\255\017\001\018\001\255\255\
\020\001\021\001\022\001\005\001\006\001\255\255\026\001\027\001\
\028\001\029\001\012\001\255\255\255\255\255\255\255\255\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\005\001\006\001\
\255\255\255\255\255\255\005\001\006\001\012\001\255\255\255\255\
\255\255\255\255\012\001\018\001\255\255\020\001\021\001\022\001\
\018\001\255\255\020\001\021\001\005\001\006\001\255\255\255\255\
\005\001\006\001\255\255\012\001\255\255\255\255\255\255\012\001\
\255\255\018\001\255\255\020\001\021\001\018\001\255\255\020\001\
\021\001\005\001\006\001\255\255\255\255\005\001\006\001\255\255\
\012\001\255\255\255\255\255\255\012\001\255\255\018\001\255\255\
\020\001\021\001\018\001\255\255\020\001\021\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  IN\000\
  CONCAT\000\
  BOOL\000\
  NAT\000\
  STRING\000\
  LPAREN\000\
  RPAREN\000\
  LCORCH\000\
  RCORCH\000\
  COMA\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  IDT\000\
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 46 "parser.mly"
      ( _1 )
# 285 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 50 "parser.mly"
      ( _1 )
# 292 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 52 "parser.mly"
            ( TmIf (_2, _4, _6) )
# 301 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 54 "parser.mly"
            ( TmAbs (_2, _4, _6) )
# 310 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
            ( TmLetIn (_2, _4, _6) )
# 319 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 58 "parser.mly"
            ( TmLetIn (_2, TmFix (TmAbs(_2, _4, _6)), _8) )
# 329 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 60 "parser.mly"
            ( TmTProj (_1, _3))
# 337 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
            ( TmRProj (_1, _3))
# 345 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'algo) in
    Obj.repr(
# 64 "parser.mly"
            ( _2 )
# 352 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 66 "parser.mly"
            ( TmDef (_1, _3) )
# 360 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 68 "parser.mly"
            ( TmTyDef (_1, _3) )
# 368 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 73 "parser.mly"
    ( TmReg ([(_1,_3)] @ _4) )
# 377 "parser.ml"
               : 'algo))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupla) in
    Obj.repr(
# 75 "parser.mly"
    ( TmTuple ([_1] @ _3))
# 385 "parser.ml"
               : 'algo))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 77 "parser.mly"
    ( TmTuple [_1])
# 392 "parser.ml"
               : 'algo))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
    ( TmTuple [])
# 398 "parser.ml"
               : 'algo))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 83 "parser.mly"
    ( [(_2,_4)] @ _5 )
# 407 "parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
    ( [] )
# 413 "parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupla) in
    Obj.repr(
# 89 "parser.mly"
      ( [_1] @ _3 )
# 421 "parser.ml"
               : 'tupla))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 91 "parser.mly"
      ( [_1] )
# 428 "parser.ml"
               : 'tupla))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 95 "parser.mly"
      ( _1 )
# 435 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 97 "parser.mly"
      ( TmSucc _2 )
# 442 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 99 "parser.mly"
      ( TmPred _2 )
# 449 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 101 "parser.mly"
      ( TmIsZero _2 )
# 456 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 103 "parser.mly"
      ( TmConcat (_2, _3) )
# 464 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 105 "parser.mly"
      ( TmApp (_1, _2) )
# 472 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 109 "parser.mly"
      ( _2 )
# 479 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
      ( TmTrue )
# 485 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
      ( TmFalse )
# 491 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
      ( TmVar _1 )
# 498 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
      ( TmVarType _1 )
# 505 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 119 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 515 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "parser.mly"
      ( TmString _1)
# 522 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 128 "parser.mly"
      ( _1 )
# 529 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 130 "parser.mly"
      ( TyArr (_1, _3) )
# 537 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 134 "parser.mly"
      ( _2 )
# 544 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "parser.mly"
      ( TyBool )
# 550 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
      ( TyNat )
# 556 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
      ( TyString )
# 562 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "parser.mly"
      (TyVar _1)
# 569 "parser.ml"
               : 'atomicTy))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.term)
