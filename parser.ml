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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 55 "parser.ml"
let yytransl_const = [|
  257 (* SPACE *);
  258 (* LAMBDA *);
  259 (* TRUE *);
  260 (* FALSE *);
  261 (* IF *);
  262 (* THEN *);
  263 (* ELSE *);
  264 (* SUCC *);
  265 (* PRED *);
  266 (* ISZERO *);
  267 (* LET *);
  268 (* LETREC *);
  269 (* IN *);
  270 (* CONCAT *);
  271 (* CAPITALIZE *);
  272 (* BOOL *);
  273 (* NAT *);
  274 (* STRING *);
  275 (* LIST *);
  276 (* FIX *);
  277 (* LPAREN *);
  278 (* RPAREN *);
  279 (* LCORCH *);
  280 (* RCORCH *);
  281 (* LBRACK *);
  282 (* RBRACK *);
  283 (* AS *);
  284 (* CASE *);
  285 (* OF *);
  286 (* BIGARROW *);
  287 (* LTAG *);
  288 (* RTAG *);
  289 (* NILLIST *);
  290 (* CONSLIST *);
  291 (* ISNILLIST *);
  292 (* HEADLIST *);
  293 (* TAILLIST *);
  294 (* COMA *);
  295 (* DOT *);
  296 (* EQ *);
  297 (* COLON *);
  298 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  299 (* INTV *);
  300 (* IDV *);
  301 (* IDT *);
  302 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\005\000\005\000\005\000\005\000\006\000\
\006\000\007\000\007\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\003\000\003\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\010\000\010\000\010\000\010\000\012\000\012\000\
\013\000\013\000\011\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\004\000\004\000\001\000\006\000\006\000\006\000\008\000\
\003\000\003\000\007\000\004\000\003\000\001\000\000\000\005\000\
\000\000\003\000\001\000\001\000\002\000\002\000\002\000\003\000\
\006\000\005\000\005\000\005\000\002\000\002\000\002\000\003\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\004\000\
\001\000\003\000\003\000\001\000\003\000\001\000\001\000\001\000\
\004\000\003\000\004\000\003\000\001\000\000\000\005\000\000\000\
\003\000\001\000\004\000\005\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\000\000\000\
\000\000\039\000\062\000\000\000\000\000\020\000\000\000\036\000\
\037\000\000\000\021\000\022\000\023\000\000\000\000\000\000\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\030\000\000\000\000\000\000\000\000\000\024\000\032\000\000\000\
\000\000\033\000\000\000\044\000\046\000\047\000\000\000\000\000\
\000\000\000\000\048\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\002\000\003\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\043\000\000\000\000\000\
\045\000\000\000\050\000\042\000\000\000\026\000\027\000\028\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\049\000\
\000\000\000\000\052\000\000\000\025\000\000\000\000\000\011\000\
\000\000\051\000\000\000\000\000\059\000\000\000\000\000\000\000\
\057\000\000\000\016\000\000\000\000\000\000\000\000\000\055\000\
\060\000"

let yydgoto = "\002\000\
\027\000\091\000\138\000\029\000\046\000\114\000\092\000\030\000\
\077\000\098\000\100\000\146\000\139\000\149\000"

let yysindex = "\011\000\
\162\255\000\000\222\254\000\000\000\000\207\255\192\000\192\000\
\192\000\234\254\242\254\192\000\192\000\184\000\207\255\140\000\
\247\254\023\255\025\255\026\255\028\255\029\255\000\000\016\255\
\017\255\000\000\000\000\002\000\192\000\000\000\020\255\000\000\
\000\000\253\254\000\000\000\000\000\000\024\255\027\255\192\000\
\000\000\192\000\241\254\030\255\243\254\041\255\031\255\223\000\
\223\000\223\000\223\000\223\000\207\255\223\000\252\254\000\000\
\000\000\223\000\207\255\207\255\223\000\000\000\000\000\207\255\
\207\255\000\000\207\255\000\000\000\000\000\000\044\255\223\000\
\000\255\032\255\000\000\046\255\033\255\047\255\048\255\051\255\
\052\255\004\000\080\000\000\000\000\000\042\255\255\254\251\254\
\049\255\003\255\008\255\000\000\237\254\223\000\065\255\053\255\
\055\255\066\255\056\255\064\255\000\000\223\000\192\000\192\000\
\192\000\192\000\000\000\000\000\207\255\207\255\207\255\207\255\
\054\255\000\000\207\255\072\255\074\255\000\000\223\000\223\000\
\000\000\223\000\000\000\000\000\192\000\000\000\000\000\000\000\
\062\255\062\255\062\255\254\254\073\255\000\000\223\000\000\000\
\067\255\076\255\000\000\078\255\000\000\207\255\207\255\000\000\
\058\255\000\000\223\000\060\255\000\000\062\255\003\255\071\255\
\000\000\077\255\000\000\223\000\223\000\067\255\078\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\093\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\009\000\000\000\000\000\000\000\055\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\067\000\000\000\116\255\097\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\098\255\000\000\000\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\103\255\105\255\000\000\000\000\000\000\000\000\000\000\
\106\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\079\000\091\000\103\000\000\000\000\000\000\000\000\000\000\000\
\107\255\108\255\000\000\101\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\115\000\103\255\000\000\
\000\000\000\000\000\000\000\000\000\000\107\255\101\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\255\255\034\000\120\000\000\000\240\255\023\000\020\000\
\000\000\000\000\000\000\241\255\253\255\243\255"

let yytablesize = 524
let yytable = "\028\000\
\036\000\056\000\059\000\107\000\034\000\110\000\063\000\111\000\
\037\000\031\000\142\000\001\000\116\000\043\000\045\000\068\000\
\069\000\070\000\071\000\055\000\072\000\038\000\073\000\055\000\
\065\000\055\000\035\000\036\000\037\000\039\000\074\000\040\000\
\041\000\055\000\047\000\055\000\055\000\055\000\084\000\085\000\
\113\000\055\000\041\000\096\000\075\000\115\000\055\000\048\000\
\057\000\049\000\050\000\082\000\051\000\052\000\004\000\053\000\
\054\000\087\000\088\000\062\000\058\000\057\000\090\000\060\000\
\066\000\093\000\031\000\061\000\094\000\064\000\067\000\101\000\
\103\000\104\000\102\000\099\000\105\000\106\000\006\000\108\000\
\109\000\076\000\078\000\079\000\080\000\081\000\118\000\083\000\
\112\000\121\000\005\000\086\000\120\000\119\000\089\000\123\000\
\122\000\133\000\135\000\136\000\055\000\152\000\007\000\154\000\
\145\000\095\000\097\000\129\000\130\000\131\000\132\000\156\000\
\143\000\147\000\008\000\148\000\015\000\157\000\036\000\036\000\
\014\000\054\000\125\000\126\000\127\000\128\000\017\000\117\000\
\019\000\053\000\056\000\058\000\061\000\042\000\155\000\124\000\
\036\000\134\000\036\000\036\000\150\000\151\000\160\000\153\000\
\141\000\161\000\000\000\000\000\036\000\000\000\000\000\000\000\
\137\000\036\000\036\000\140\000\000\000\000\000\036\000\036\000\
\036\000\036\000\000\000\003\000\004\000\005\000\006\000\000\000\
\144\000\007\000\008\000\009\000\010\000\011\000\000\000\012\000\
\013\000\000\000\000\000\000\000\000\000\014\000\015\000\000\000\
\016\000\000\000\000\000\000\000\000\000\158\000\159\000\000\000\
\017\000\000\000\018\000\019\000\020\000\021\000\022\000\000\000\
\000\000\000\000\000\000\000\000\023\000\024\000\025\000\026\000\
\003\000\004\000\005\000\006\000\000\000\000\000\007\000\008\000\
\009\000\010\000\011\000\000\000\012\000\013\000\000\000\000\000\
\000\000\000\000\014\000\015\000\000\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\017\000\000\000\018\000\
\019\000\020\000\021\000\022\000\000\000\000\000\000\000\000\000\
\000\000\023\000\032\000\033\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\036\000\000\000\036\000\
\000\000\000\000\000\000\000\000\000\000\037\000\000\000\037\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\036\000\
\055\000\037\000\055\000\036\000\036\000\036\000\036\000\037\000\
\041\000\041\000\000\000\037\000\037\000\037\000\037\000\041\000\
\000\000\000\000\000\000\000\000\004\000\004\000\000\000\000\000\
\041\000\000\000\041\000\004\000\041\000\000\000\000\000\000\000\
\031\000\031\000\041\000\000\000\004\000\000\000\004\000\031\000\
\041\000\041\000\041\000\000\000\006\000\006\000\004\000\000\000\
\031\000\000\000\031\000\006\000\004\000\004\000\000\000\000\000\
\005\000\005\000\031\000\000\000\006\000\000\000\006\000\005\000\
\031\000\031\000\000\000\000\000\007\000\007\000\006\000\000\000\
\005\000\000\000\005\000\007\000\006\000\000\000\000\000\000\000\
\008\000\008\000\005\000\000\000\007\000\000\000\007\000\008\000\
\005\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\008\000\000\000\008\000\000\000\007\000\003\000\004\000\005\000\
\006\000\000\000\008\000\007\000\008\000\009\000\010\000\011\000\
\008\000\012\000\013\000\000\000\000\000\000\000\000\000\014\000\
\015\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\000\000\018\000\019\000\020\000\021\000\
\022\000\000\000\000\000\000\000\000\000\000\000\023\000\044\000\
\033\000\026\000\004\000\005\000\000\000\000\000\000\000\007\000\
\008\000\009\000\004\000\005\000\000\000\012\000\013\000\000\000\
\000\000\000\000\000\000\014\000\015\000\000\000\016\000\000\000\
\000\000\000\000\000\000\000\000\015\000\000\000\016\000\000\000\
\018\000\019\000\020\000\021\000\022\000\000\000\000\000\000\000\
\018\000\000\000\023\000\032\000\033\000\026\000\000\000\000\000\
\000\000\000\000\023\000\032\000\033\000\026\000\068\000\069\000\
\070\000\071\000\000\000\072\000\000\000\073\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\074\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\075\000"

let yycheck = "\001\000\
\000\000\000\000\006\001\000\000\006\000\007\001\022\001\013\001\
\000\000\044\001\013\001\001\000\032\001\015\000\016\000\016\001\
\017\001\018\001\019\001\039\001\021\001\044\001\023\001\039\001\
\038\001\039\001\007\000\008\000\009\000\044\001\031\001\012\000\
\013\000\039\001\044\001\039\001\039\001\039\001\043\001\044\001\
\038\001\039\001\000\000\044\001\045\001\038\001\039\001\025\001\
\029\000\025\001\025\001\053\000\025\001\025\001\000\000\040\001\
\040\001\059\000\060\000\040\000\041\001\042\000\064\000\040\001\
\024\001\067\000\000\000\041\001\025\001\040\001\040\001\026\001\
\026\001\026\001\042\001\044\001\026\001\026\001\000\000\000\000\
\039\001\048\000\049\000\050\000\051\000\052\000\022\001\054\000\
\040\001\024\001\000\000\058\000\038\001\041\001\061\000\032\001\
\041\001\044\001\027\001\026\001\039\001\044\001\000\000\044\001\
\038\001\072\000\073\000\109\000\110\000\111\000\112\000\041\001\
\040\001\038\001\000\000\038\001\024\001\041\001\003\001\004\001\
\024\001\024\001\103\000\104\000\105\000\106\000\024\001\094\000\
\024\001\024\001\024\001\024\001\032\001\014\000\151\000\102\000\
\021\001\115\000\023\001\024\001\142\000\143\000\158\000\147\000\
\125\000\159\000\255\255\255\255\033\001\255\255\255\255\255\255\
\119\000\038\001\039\001\122\000\255\255\255\255\043\001\044\001\
\045\001\046\001\255\255\002\001\003\001\004\001\005\001\255\255\
\135\000\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\255\255\255\255\255\255\255\255\020\001\021\001\255\255\
\023\001\255\255\255\255\255\255\255\255\156\000\157\000\255\255\
\031\001\255\255\033\001\034\001\035\001\036\001\037\001\255\255\
\255\255\255\255\255\255\255\255\043\001\044\001\045\001\046\001\
\002\001\003\001\004\001\005\001\255\255\255\255\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\015\001\255\255\255\255\
\255\255\255\255\020\001\021\001\255\255\023\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\031\001\255\255\033\001\
\034\001\035\001\036\001\037\001\255\255\255\255\255\255\255\255\
\255\255\043\001\044\001\045\001\046\001\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\021\001\255\255\023\001\
\255\255\255\255\255\255\255\255\255\255\021\001\255\255\023\001\
\255\255\033\001\255\255\255\255\255\255\255\255\255\255\039\001\
\039\001\033\001\039\001\043\001\044\001\045\001\046\001\039\001\
\006\001\007\001\255\255\043\001\044\001\045\001\046\001\013\001\
\255\255\255\255\255\255\255\255\006\001\007\001\255\255\255\255\
\022\001\255\255\024\001\013\001\026\001\255\255\255\255\255\255\
\006\001\007\001\032\001\255\255\022\001\255\255\024\001\013\001\
\038\001\039\001\040\001\255\255\006\001\007\001\032\001\255\255\
\022\001\255\255\024\001\013\001\038\001\039\001\255\255\255\255\
\006\001\007\001\032\001\255\255\022\001\255\255\024\001\013\001\
\038\001\039\001\255\255\255\255\006\001\007\001\032\001\255\255\
\022\001\255\255\024\001\013\001\038\001\255\255\255\255\255\255\
\006\001\007\001\032\001\255\255\022\001\255\255\024\001\013\001\
\038\001\255\255\255\255\255\255\255\255\255\255\032\001\255\255\
\022\001\255\255\024\001\255\255\038\001\002\001\003\001\004\001\
\005\001\255\255\032\001\008\001\009\001\010\001\011\001\012\001\
\038\001\014\001\015\001\255\255\255\255\255\255\255\255\020\001\
\021\001\255\255\023\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\031\001\255\255\033\001\034\001\035\001\036\001\
\037\001\255\255\255\255\255\255\255\255\255\255\043\001\044\001\
\045\001\046\001\003\001\004\001\255\255\255\255\255\255\008\001\
\009\001\010\001\003\001\004\001\255\255\014\001\015\001\255\255\
\255\255\255\255\255\255\020\001\021\001\255\255\023\001\255\255\
\255\255\255\255\255\255\255\255\021\001\255\255\023\001\255\255\
\033\001\034\001\035\001\036\001\037\001\255\255\255\255\255\255\
\033\001\255\255\043\001\044\001\045\001\046\001\255\255\255\255\
\255\255\255\255\043\001\044\001\045\001\046\001\016\001\017\001\
\018\001\019\001\255\255\021\001\255\255\023\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\031\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\045\001"

let yynames_const = "\
  SPACE\000\
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
  CAPITALIZE\000\
  BOOL\000\
  NAT\000\
  STRING\000\
  LIST\000\
  FIX\000\
  LPAREN\000\
  RPAREN\000\
  LCORCH\000\
  RCORCH\000\
  LBRACK\000\
  RBRACK\000\
  AS\000\
  CASE\000\
  OF\000\
  BIGARROW\000\
  LTAG\000\
  RTAG\000\
  NILLIST\000\
  CONSLIST\000\
  ISNILLIST\000\
  HEADLIST\000\
  TAILLIST\000\
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
# 63 "parser.mly"
      ( _1 )
# 403 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 65 "parser.mly"
      ( TmDef (_1, _3) )
# 411 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 67 "parser.mly"
      ( TmTyDef (_1, _3) )
# 419 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( _1 )
# 426 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 73 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 435 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 75 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 444 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 77 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 453 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 79 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs(_2, _4, _6)), _8) )
# 463 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 81 "parser.mly"
      ( TmTProj (_1, _3))
# 471 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
      ( TmRProj (_1, _3))
# 479 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 85 "parser.mly"
      ( TmVariant (_2, _4, _7))
# 488 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 90 "parser.mly"
    ( TmReg ([(_1,_3)] @ _4) )
# 497 "parser.ml"
               : 'algo))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupla) in
    Obj.repr(
# 92 "parser.mly"
    ( TmTuple ([_1] @ _3))
# 505 "parser.ml"
               : 'algo))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 94 "parser.mly"
    ( TmTuple [_1])
# 512 "parser.ml"
               : 'algo))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
    ( TmTuple [])
# 518 "parser.ml"
               : 'algo))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 100 "parser.mly"
    ( [(_2,_4)] @ _5 )
# 527 "parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
    ( [] )
# 533 "parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupla) in
    Obj.repr(
# 106 "parser.mly"
      ( [_1] @ _3 )
# 541 "parser.ml"
               : 'tupla))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 108 "parser.mly"
      ( [_1] )
# 548 "parser.ml"
               : 'tupla))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 112 "parser.mly"
      ( _1 )
# 555 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 114 "parser.mly"
      ( TmSucc _2 )
# 562 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 116 "parser.mly"
      ( TmPred _2 )
# 569 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 118 "parser.mly"
      ( TmIsZero _2 )
# 576 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 120 "parser.mly"
      ( TmConcat (_2, _3) )
# 584 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 122 "parser.mly"
      ( TmList (_3, _5, _6))
# 593 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 124 "parser.mly"
      ( TmIsNil (_3, _5))
# 601 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 126 "parser.mly"
      ( TmHeadList (_3, _5))
# 609 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 128 "parser.mly"
      ( TmTailList (_3, _5))
# 617 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 130 "parser.mly"
      ( TmCapitalize (_2))
# 624 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 132 "parser.mly"
      ( TmApp (_1, _2) )
# 632 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 134 "parser.mly"
      ( TmFix _2 )
# 639 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 139 "parser.mly"
      ( _2 )
# 646 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'algo) in
    Obj.repr(
# 141 "parser.mly"
      ( _2 )
# 653 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
      ( TmTrue )
# 659 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "parser.mly"
      ( TmFalse )
# 665 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 147 "parser.mly"
      ( TmVar _1 )
# 672 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 149 "parser.mly"
      ( TmVarType _1 )
# 679 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 151 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 689 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
      ( TmString _1)
# 696 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 158 "parser.mly"
      ( TmEmptyList _3 )
# 703 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 162 "parser.mly"
      ( _1 )
# 710 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 164 "parser.mly"
      ( TyArr (_1, _3) )
# 718 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 168 "parser.mly"
    ( _2 )
# 725 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "parser.mly"
    ( TyBool )
# 731 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'algoty) in
    Obj.repr(
# 172 "parser.mly"
    ( _2 )
# 738 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "parser.mly"
    ( TyNat )
# 744 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "parser.mly"
    ( TyString )
# 750 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 178 "parser.mly"
    ( TyVar _1 )
# 757 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 180 "parser.mly"
    ( TyList _3 )
# 764 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'variantTy2) in
    Obj.repr(
# 182 "parser.mly"
    ( _2 )
# 771 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'regty) in
    Obj.repr(
# 186 "parser.mly"
    ( TyReg ([(_1,_3)] @ _4) )
# 780 "parser.ml"
               : 'algoty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuplaty) in
    Obj.repr(
# 188 "parser.mly"
    ( TyTuple ([_1] @ _3))
# 788 "parser.ml"
               : 'algoty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 190 "parser.mly"
    ( TyTuple [_1])
# 795 "parser.ml"
               : 'algoty))
; (fun __caml_parser_env ->
    Obj.repr(
# 192 "parser.mly"
    ( TyTuple [])
# 801 "parser.ml"
               : 'algoty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'regty) in
    Obj.repr(
# 196 "parser.mly"
    ( [(_2,_4)] @ _5 )
# 810 "parser.ml"
               : 'regty))
; (fun __caml_parser_env ->
    Obj.repr(
# 198 "parser.mly"
    ( [] )
# 816 "parser.ml"
               : 'regty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuplaty) in
    Obj.repr(
# 202 "parser.mly"
      ( [_1] @ _3 )
# 824 "parser.ml"
               : 'tuplaty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 204 "parser.mly"
      ( [_1] )
# 831 "parser.ml"
               : 'tuplaty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 208 "parser.mly"
    ( TyVariant ([(_1,_3)] @ _4) )
# 840 "parser.ml"
               : 'variantTy2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 212 "parser.mly"
    ( [(_2,_4)] @ _5 )
# 849 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    Obj.repr(
# 214 "parser.mly"
    ( [] )
# 855 "parser.ml"
               : 'expr2))
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
