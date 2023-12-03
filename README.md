# DLP
### Memoria

**1.1 Implementacion reconocimiento expresiones multi-linea.**
Mediante la funcion read_command del fichero main.ml

**2.1 Incorporación de un combinador de punto fijo interno.**

Agregando:

# Lambda

lambda.mli linea:25
```
| TmFix of term
```

lambda.ml linea:27
```
| TmFix of term
```
lambda.ml linea:129
```
| TmFix t1 ->
   		let tyT1 = typeof ctx t1 in
   		(match tyT1 with
   			TyArr (tyT11, tyT12) ->
   				if tyT11 = tyT12 then tyT12
   				else raise (Type_error "result of body not compatible with domain")
   		| 	_ -> raise (Type_error "arrow type expected"))
```

lambda.ml linea:179
```
| TmFix t ->
  	  "(fix " ^ string_of_term t ^ ")"
```

lambda.ml linea:220
```
| TmFix t ->
  	  free_vars t
```

lambda.ml linea:265
```
| TmFix t ->
  	  TmFix (subst x s t)
```

lambda.ml linea:360
```
| TmFix (TmAbs (x, _, t2)) ->
  	  subst x tm t2
```

lambda.ml linea:364
```
| TmFix t1 ->
  	let t1' = eval1 t1 in
  	TmFix t1'
```

# Lexer:

lexer.mll linea:20
```
| "letrec"	  { LETREC }
```

# Parser:

parser.mly linea:16
```
%token LETREC
```

parser.mlñy linea:53
```
| LETREC IDV COLON ty EQ term IN term
  	  { TmLetIn ($2, TmFix (TmAbs($2, $4, $6)), $8) }
```

**2.3 Incorporacion del tipo String para el soporte decadenas de caracteres, ası como de la operacion de concatenacion de estas cadenas.**

# Lambda:

lambda.mli linea:6
```
| TyString
```

lambda.mli linea:26
```
| TmString of string
| TmConcat of term * term
```

lambda.ml linea:8
```
| TyString
```

lambda.ml linea:28
```
  | TmString of string
  | TmConcat of term * term
```

lambda.ml linea:57
```
| TyString ->
      "String"
```

lambda.ml linea:138
```
  | TmString _->
      TyString
```

lambda.ml linea:142
```
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "argument of concat is not a string")
```

lambda.ml linea:181
```
| TmString s ->
      "\"" ^ s ^ "\""
```

lambda.ml linea:183
```
| TmConcat (t1, t2) ->
      "concat " ^ "(" ^ string_of_term t1 ^ ")" ^ " " ^ "(" ^ string_of_term t2 ^ ")"
```

lambda.ml linea:222
```
  | TmString _ ->
    []
  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
```

lambda.ml linea:267
```
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
```

lambda.ml linea:283
```
| TmString _ -> true
```

lambda.ml linea:369
```
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
```

lambda.ml linea:373
```
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 t2 in
      TmConcat (TmString s1, t2')
```

lambda.ml linea:378
```
  | TmConcat (t1, s2) ->
      let t1' = eval1 t1 in
      TmConcat (t1', s2)
```
# Lexer:

lexer.mll linea:25
```
  | "String"    { STRING }
```

lexer.mll linea:35
```
  | '"'[^ '"' ';' '\n']* '"'
                { let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s - 2))}
```
# Parser:

parser.mly linea:21
```
%token STRING
```

parser.mly linea:33
```
%token <string> STRINGV
```

parser.mly linea:65
```
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
```

parser.mly linea:84
```
  | STRINGV
      { TmString $1}
```

parser.mly linea:100
```
  | STRING
      { TyString }
```

**2.2 Incorporacion de un contexto de definiciones globales**

# Lexer

Lexer.mly linea:33
Se añadio 'A'-'Z'
```
| ['a'-'z''A'-'Z']['a'-'z' '_' '0'-'9']*
```

# Parser 

Se añadio
Parser.mly linea:59

```
tyTerms :
    atomicTyTerms
      { $1 }
  | atomicTyTerms ARROW tyTerms
      { TmTyArr ($1, $3) }

atomicTyTerms :
    LPAREN tyTerms RPAREN
      { $2 }
  | BOOL
      { TmTyBool }
  | NAT
      { TmTyNat }
  | STRING
      { TmTyString }
  | IDV
      { TmVar $1 }
```

Parser.mly linea:90
```
  | tyTerms 
    { $1 }
```

Parser.mly linea:125
```
| IDV
      { TyVar $1 }
```

# lambda.mli

lambda.mli linea:7
```
  | TyVar of string
```

lambda.mli linea:30
```
  | TmDef of string * term
  | TmTyBool
  | TmTyNat
  | TmTyArr of term * term
  | TmTyString
```

lambda.mli linea:40
```
type contextTerm =
  (string * term) list
;;
```

lambda.mli linea: 48
```
val emptyctxTerms : contextTerm;;
val addbindingTerms : contextTerm -> string -> term -> contextTerm;;
val getbindingTerms : contextTerm -> string -> term;;

```

# lambda.ml
*Las mismas cabeceras que en lambda.mli*

Y tambien...

lambda.ml linea:47
```
let emptyctxTerms = 
  []
;;

let addbindingTerms ctx x bind = 
  (x, bind):: ctx
;;

let getbindingTerms ctx x = 
  List.assoc x ctx
;;
```

lambda.ml linea:82 (Dentro de string_of_ty)
```
| TyVar t -> t
```

lambda.ml linea:122 (Dentro de string_of_term)
```
| TmDef (t1, t2) ->
      t1 ^ " = " ^ string_of_term t2
  | TmTyArr (t1,t2) -> 
      string_of_term t1 ^ "->" ^ string_of_term t2
  | TmTyBool -> 
    "Bool"
  | TmTyNat -> 
    "Nat"
  | TmTyString -> 
    "String"
```
añadido a la cabecera de typeof el contexto de terminos global

lambda.ml linea:186 (Dentro de typeof, modificado parcialmente, añadiendo el match para obtener el tipo de entrada de la abstraccion, dependiendo de si ya es un tipo o es una variable TyVar de tipo)
```
| TmAbs (x, tyT1, t2) ->
let typesCtx' = addbinding typesCtx x tyT1 in
      let termsCtx' = addbindingTerms termsCtx x t2 in
      let tyT2 = typeof typesCtx' termsCtx' t2 in
      let tyT1' = 
      (match tyT1 with
        | TyVar t -> typeof typesCtx' termsCtx' ((getbindingTerms termsCtx (string_of_ty(tyT1))) )
        | _ -> tyT1) in
      TyArr (tyT1', tyT2)
```

lambda.ml linea:231 (Dentro de typeof)
```
| TmDef (x, t1) ->
      let tyT1 = typeof typesCtx termsCtx t1 in
      let ctx' = addbinding typesCtx x tyT1 in
      let termsCtx' = addbindingTerms termsCtx x t1 in
      typeof ctx' termsCtx' t1

  | TmTyArr (t1,t2) ->
    let tyT1 = typeof typesCtx termsCtx t1 in
    let tyT2 = typeof typesCtx termsCtx t2 in
    TyArr (tyT1,tyT2)

  | TmTyBool -> 
    TyBool

  | TmTyNat -> TyNat

  | TmTyString -> TyString
```

lambda.ml linea:291 (Dentro de free_vars)
```
| _ ->  []
```

lambda.ml linea 339 (Dentro de subst)
```
| _ -> tm
```

lambda.ml linea:362 
```
let esAbstraccion termsCtx = function
  | TmAbs (_,_,_) -> true
  | _ -> false
let devolverAbstraccion termsCtx typesCtx (TmAbs(y,ty,t12)) = 
  match (ty,t12) with
  | (TyVar t1, TmVar t) -> TmAbs (y, (getbinding typesCtx (string_of_ty(ty))), (getbindingTerms termsCtx (string_of_term(t12))))
  | (_, TmVar t) -> TmAbs (y, ty, (getbindingTerms termsCtx (string_of_term(t12))))
  | (TyVar t1, _) -> TmAbs (y, (getbinding typesCtx (string_of_ty(ty))), t12)
  | (_,_) -> (TmAbs(y,ty,t12))

let esArrowType termsCtx = function
  | TyArr _ -> true
  | _ -> false
```

lambda.ml linea:503
```
| TmDef (x, t1) when isval t1->
    print_endline("PASA POR AQUI 11");
    print_endline(string_of_term(t1));
    if esAbstraccion termsCtx t1 then devolverAbstraccion termsCtx typesCtx t1 else t1

  | TmDef (x, t1) ->
    print_endline("PASA POR AQUI 10");
    let t1' = eval1 termsCtx typesCtx t1 in TmDef (x, t1')
```

Se añadio termsCtx typesCtx a la cabecera de eval1 y de eval

# main.ml

Dentro del loop principal:
```
let tm = s token (from_string (read_command ())) in
      
      if esDefinicion tm 
        then  let tyTm = typeof typesCtx termsCtx tm in 
              
              let nombreVar = String.split_on_char ' ' (string_of_term(tm)) in
              if comienza_con_mayuscula (List.nth nombreVar 0)
                then print_endline("type " ^ (List.nth nombreVar 0) ^ " = " ^ string_of_ty tyTm)
                else print_endline((List.nth nombreVar 0) ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term (eval termsCtx typesCtx tm));
              
              
              loop (addbinding typesCtx (List.nth nombreVar 0) tyTm) (addbindingTerms termsCtx (List.nth nombreVar 0) (eval termsCtx typesCtx tm))
        else  let tyTm = typeof typesCtx termsCtx tm in
              let nombreVar = String.split_on_char ' ' (string_of_term(tm)) in
              if comienza_con_mayuscula (List.nth nombreVar 0)
                then print_endline("type " ^ (List.nth nombreVar 0) ^ " = " ^ string_of_ty tyTm)
                else print_endline("-: " ^ string_of_ty tyTm ^ " = " ^ string_of_term (eval termsCtx typesCtx tm));
              
              loop typesCtx termsCtx
```

```
let esDefinicion = function
  | TmDef (_,_) -> true
  | _ -> false


let comienza_con_mayuscula (cadena : string) : bool =
  let patron = Str.regexp "^[A-Z]" in
  try
    ignore (Str.search_forward patron cadena 0);
    true
  with Not_found -> false
```

```
| Not_found ->
        print_endline "Otro error";
        loop typesCtx termsCtx
```
 
Se le añadio emptyctxTerms a la cabecera del main

# Makefile

Se añadio str.cma a la linea 3:
ocamlc str.cma -o top lambda.cmo parser.cmo lexer.cmo main.cmo 
