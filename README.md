# DLP
### Memoria

**1.1 Implementacion reconocimiento expresiones multi-linea.**
Mediante la funcion read_command del fichero main.ml

**2.1 Incorporación de un combinador de punto fijo interno.**

Agregando:
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

#Lambda.mli
lambda.mli linea:25
```
| TmFix of term
```

#Lexer:
lexer.mll linea:20
```
| "letrec"	  { LETREC }
```

#Parser:
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
#Lambda
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
#Lexer
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
#Parser
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