
%{
  open Lambda;;
%}
%token SPACE
%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token CAPITALIZE
%token BOOL
%token NAT
%token STRING
%token LIST
%token FIX


%token LPAREN
%token RPAREN
%token LCORCH
%token RCORCH
%token LBRACK
%token RBRACK
%token AS
%token CASE
%token OF
%token BIGARROW
%token LTAG
%token RTAG
%token NILLIST
%token CONSLIST
%token ISNILLIST
%token HEADLIST
%token TAILLIST
%token COMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV

%start s
%type <Lambda.term> s

%%

s :
    term EOF
      { $1 }

term :
    appTerm
      { $1 }
        | IF term THEN term ELSE term
            { TmIf ($2, $4, $6) }
        | LAMBDA IDV COLON ty DOT term
            { TmAbs ($2, $4, $6) }
        | LET IDV EQ term IN term
            { TmLetIn ($2, $4, $6) }
        | LETREC IDV COLON ty EQ term IN term
            { TmLetIn ($2, TmFix (TmAbs($2, $4, $6)), $8) }
        | term DOT INTV
            { TmTProj ($1, $3)}
        | term DOT IDV
            { TmRProj ($1, $3)}
        | LTAG IDV EQ term RTAG AS ty
            { TmVariant ($2, $4, $7)}
        | IDV EQ term
            { TmDef ($1, $3) }
        | IDT EQ ty
            { TmTyDef ($1, $3) }


algo:
  | IDV EQ term reg
    { TmReg ([($1,$3)] @ $4) }
  | term COMA tupla
    { TmTuple ([$1] @ $3)}
  | term
    { TmTuple [$1]}
  | /*Tupla vacia*/
    { TmTuple []}

reg:
  | COMA IDV EQ term reg
    { [($2,$4)] @ $5 }
  | /**/
    { [] }

tupla:
  | term COMA tupla
      { [$1] @ $3 }
  | term
      { [$1] }
        
appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
  | CONSLIST LBRACK ty RBRACK atomicTerm atomicTerm
      { TmList ($3, $5, $6)}
  | ISNILLIST LBRACK ty RBRACK atomicTerm
      { TmIsNil ($3, $5)}
  | HEADLIST LBRACK ty RBRACK atomicTerm
      { TmHeadList ($3, $5)}
  | TAILLIST LBRACK ty RBRACK atomicTerm
      { TmTailList ($3, $5)}
  | CAPITALIZE atomicTerm
      { TmCapitalize ($2)}
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | FIX appTerm
      { TmFix $2 }
  

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LCORCH algo RCORCH
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | IDT
      { TmVarType $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1}
  | NILLIST LBRACK ty RBRACK
      { TmEmptyList $3 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN
    { $2 }
  | BOOL
    { TyBool }
  | LCORCH algoty RCORCH
    { $2 }
  | NAT
    { TyNat }
  | STRING
    { TyString }
  | IDT 
    { TyVar $1 }
  | LIST LBRACK ty RBRACK
    { TyList $3 }
  | LTAG variantTy2 RTAG
    { $2 }

algoty:
  | IDV COLON ty regty
    { TyReg ([($1,$3)] @ $4) }
  | ty COMA tuplaty
    { TyTuple ([$1] @ $3)}
  | ty
    { TyTuple [$1]}
  | /*Tupla vacia*/
    { TyTuple []}

regty:
  | COMA IDV COLON ty regty
    { [($2,$4)] @ $5 }
  | /**/
    { [] }

tuplaty:
  | ty COMA tuplaty
      { [$1] @ $3 }
  | ty
      { [$1] }

variantTy2:
  | IDV COLON ty expr2
    { TyVariant ([($1,$3)] @ $4) }

expr2:
  | COMA IDV COLON ty expr2
    { [($2,$4)] @ $5 }
  | /**/
    { [] }


  
