# Interprete de lambda calculo
## Autores

Juan Villaverde

Carlos Rodriguez rojo - carlos.rojo@udc.es

## Manual de usuario
### **1.1 Implementacion reconocimiento expresiones multi-linea.**
El interprete reconoce expresiones hasta que encuentra dos puntos y coma seguidos '**;;**'.

### **2.1 Incorporación de un combinador de punto fijo interno.**

Es posible funciones recursivas mediante la palabra clave **letrec**, de forma que en lugar de escribir:
```
let fix = lambda f.(lambda x. f (lambda y. x x y)) (lambda x. f (lambda y. x x y)) in
let sumaux =
lambda f. (lambda n. (lambda m. if (iszero n) then m else succ (f (pred n) m))) in
let sum = fix sumaux in
sum 21 34
```
Puedes escribir:
```
letrec sum : Nat -> Nat -> Nat =
lambda n : Nat. lambda m : Nat. if iszero n then m else succ (sum (pred n) m) in
sum 21 34
```

### **2.2 Incorporacion de un contexto de definiciones globales**

Permite asociar nombres de variables con valores o terminos asi como la creacion de alias de tipos de la misma manera:

```
identificador = termino
x = false;;
N = Nat;;
lambda x: N. x;;
```

### **2.3 Incorporacion del tipo string**

Se incorpora el tipo string de forma que se puedan formalizar cadenas de caracteres asi como la concatenacion de strings y...

Para formalizar un string se hace mediante las dobles comillas de la siguiente manera:

```
"srt";;
"Hola Mundo";;
```

Para la concatenacion se utiliza la palabre clave **concat**:
```
concat "hola " "mundo";;
```
A mayores, se añadio una funcion que permite poner a mayusculas un string, dicha funcion se utiliza con la palabra clave **capitalize**

```
capitalize (concat "hola " "mundo";;)
```

### **2.4 Incorporación del tipo tuplas**

Mediante los corchetes se puede definir el tipo tuplas siendo **{}**
la tupla vacia. Tambien se pueden proyectar sus elementos mediante un punto y un entero empezando desde el 0:
```
{1,"hola mundo", if true then false else true} : {Nat, String, Bool} 
{3,4}.1 --> devolveria 4
```

### **2.5 Incorporación del tipo registros**

Para definir un registro tambien se utilizan los corchetes y una etiqueta asociada a cada termino de la siguiente manera:
```
{hola=2, mundo="srt",adios=true} : {hola:Nat, mundo:String, adios:Bool};;
```

Para su proyeccion se utiliza la etiqueta:

```
{hola=2, mundo="srt"}.hola ---> 2
```

No se contempla el registro vacio, **{}** sigue siendo una tupla vacía.

### **2.7 Incorporacion de variantes**

Para definir las variantes se hace uso de los simbolos de etiqueta: "<>"
Antes de definir un termino, debemos definir un tipo de variantes.


```
Int = <pos:Nat, zero:Bool, neg:Nat>;;
```
Posteriormente, podemos acceder a cada termino dentro de ese tipo de la siguiente manera:

```
p3 = <pos=3> as Int;;
```



### **2.8 Incorporacion de listas**

Tambien se ha incorporado la posibilidad de definir y trabajar con listas
La sintaxis y operaciones para ello es la siguiente:

T => Tipo de dato (Nat, Bool, Nat -> Bool...)
l => Termino de tipo lista
t => Termino

Crear una lista vacia: nil[T]
```
nil[Nat];;
```

Crear una lista con elementos: cons[T] t l
```
cons[Nat] 3 nil[Nat];;
```

Comprobar si una lista esta vacia: isnil[T] l
```
isnil[Nat] (nil[Nat]);;
isnil[Nat] (cons[Nat] 3 nil[Nat]);;
```

Obtener la cabeza de una lista: head[T] l
```
head[Nat] (cons[Nat] 3 nil[Nat]);;
```

Obtener la cola de una lista: tail[T] l
```
tail[Nat] (cons[Nat] 5 (cons[Nat] 3 (cons[Nat] 9 nil[Nat])));;
```

## Manual tecnico

Cambios realizados en los modulos con cada incorporacion:

### **1.1 Reconocimiento de expresiones multilinea**
#### Modulo: main.ml
linea:8
```
let read_command () =
  let rec read acc =
    try
      let line = read_line () in
      if String.ends_with ~suffix:";;" line
        then String.concat " " (List.rev (String.sub line 0 (String.length line - 2)::acc))
        else read(line::acc)
    with End_of_file ->
      String.concat " " (List.rev acc)
    in read []
```

linea:39
```
let tm = s token (from_string (read_command ())) in
```

### **2.1 Incorporacion de un punto fijo interno**
#### Modulo:lambda.mli

linea:25
```
| TmFix of term
```

#### Modulo:lambda.ml
linea:27
```
| TmFix of term
```
linea:129
```
| TmFix t1 ->
   		let tyT1 = typeof ctx t1 in
   		(match tyT1 with
   			TyArr (tyT11, tyT12) ->
   				if tyT11 = tyT12 then tyT12
   				else raise (Type_error "result of body not compatible with domain")
   		| 	_ -> raise (Type_error "arrow type expected"))
```

linea:179
```
| TmFix t ->
  	  "(fix " ^ string_of_term t ^ ")"
```

linea:220
```
| TmFix t ->
  	  free_vars t
```

linea:265
```
| TmFix t ->
  	  TmFix (subst x s t)
```
linea:360
```
| TmFix (TmAbs (x, _, t2)) ->
  	  subst x tm t2
```

linea:364
```
| TmFix t1 ->
  	let t1' = eval1 t1 in
  	TmFix t1'
```

#### Modulo lexer.mll:

linea:20
```
| "letrec"	  { LETREC }
```

#### Modulo parser.mly:

linea:16
```
%token LETREC
```

linea:53
```
| LETREC IDV COLON ty EQ term IN term
  	  { TmLetIn ($2, TmFix (TmAbs($2, $4, $6)), $8) }
```

### **2.2 Incorporacion de un contexto de definiciones globales**

#### Modulo lexer.mll

linea:33
Se añadio 'A'-'Z'
```
| ['a'-'z''A'-'Z']['a'-'z' '_' '0'-'9']*
```

#### Modulo parser.mly 

linea:59
Se añadio
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

linea:90
```
  | tyTerms 
    { $1 }
```

linea:125
```
| IDV
      { TyVar $1 }
```

#### Modulo lambda.mli

linea:7
```
  | TyVar of string
```
linea:30
```
  | TmDef of string * term
  | TmTyBool
  | TmTyNat
  | TmTyArr of term * term
  | TmTyString
```
linea:40
```
type contextTerm =
  (string * term) list
;;
```
linea: 48
```
val emptyctxTerms : contextTerm;;
val addbindingTerms : contextTerm -> string -> term -> contextTerm;;
val getbindingTerms : contextTerm -> string -> term;;

```

#### Modulo lambda.ml
*Las mismas cabeceras que en lambda.mli*

Y tambien...

linea:47
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
linea:82 (Dentro de string_of_ty)
```
| TyVar t -> t
```
 linea:122 (Dentro de string_of_term)
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

linea:186 (Dentro de typeof, modificado parcialmente, añadiendo el match para obtener el tipo de entrada de la abstraccion, dependiendo de si ya es un tipo o es una variable TyVar de tipo)
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

linea:231 (Dentro de typeof)
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

linea:291 (Dentro de free_vars)
```
| _ ->  []
```
linea 339 (Dentro de subst)
```
| _ -> tm
```

linea:362 
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

linea:503
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

#### Modulo main.ml

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

#### Makefile

Se añadio str.cma a la linea 3:
ocamlc str.cma -o top lambda.cmo parser.cmo lexer.cmo main.cmo 

### **2.3 Incorporacion del tipo String para el soporte decadenas de caracteres, ası como de la operacion de concatenacion de estas cadenas.**

#### Modulo lambda.mli:

linea:6
```
| TyString
```

linea:26
```
| TmString of string
| TmConcat of term * term
```

#### Modulo lambda.ml
linea:8
```
| TyString
```

linea:28
```
  | TmString of string
  | TmConcat of term * term
```
linea:57
```
| TyString ->
      "String"
```

linea:138
```
  | TmString _->
      TyString
```

linea:142
```
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "argument of concat is not a string")
```

linea:181
```
| TmString s ->
      "\"" ^ s ^ "\""
```

linea:183
```
| TmConcat (t1, t2) ->
      "concat " ^ "(" ^ string_of_term t1 ^ ")" ^ " " ^ "(" ^ string_of_term t2 ^ ")"
```

linea:222
```
  | TmString _ ->
    []
  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
```
linea:267
```
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
```

linea:283
```
| TmString _ -> true
```
linea:369
```
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
```
linea:373
```
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 t2 in
      TmConcat (TmString s1, t2')
```
linea:378
```
  | TmConcat (t1, s2) ->
      let t1' = eval1 t1 in
      TmConcat (t1', s2)
```
#### Modulo lexer.mll:

linea:25
```
  | "String"    { STRING }
```

linea:35
```
  | '"'[^ '"' ';' '\n']* '"'
                { let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s - 2))}
```
#### Modulo parser.mly:

linea:21
```
%token STRING
```

linea:33
```
%token <string> STRINGV
```

linea:65
```
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
```

linea:84
```
  | STRINGV
      { TmString $1}
```

linea:100
```
  | STRING
      { TyString }
```

### 2.4 Incorporacion de tuplas
#### Modulo: lexer.mll
linea 42
```
  | '{'         { LCORCH }
  | '}'         { RCORCH }
  | ','         { COMA }
```
#### Modulo: parser.mly
linea 29
```
%token LCORCH
%token RCORCH
%token COMA
```
```
    | LCORCH algo RCORCH
         { $2 }
```
```
algo:
  | term COMA tupla
    { TmTuple ([$1] @ $3)}
  | term
    { TmTuple [$1]}
  | /*Tupla vacia*/
    { TmTuple []}
```
```
        | term DOT INTV
            { TmTProj ($1, $3)}
```
#### Modulo: lambda.mli
linea 2:
```
  | TyTuple of ty list
```
linea 36
```
  | TmTuple of term list
  | TmTProj of term * int
```
#### Modulo: lambda.ml
linea 10:
```
  | TyTuple of ty list
```
linea 38
```
  | TmTuple of term list
  | TmTProj of term * int
```
linea 97
```
| TyTuple l ->
      let rec aux str=function
        | [] -> "{" ^ str ^ "}"
        | [h] -> aux (str ^ string_of_ty h) []
        | h::t -> aux (str ^ string_of_ty h ^ ", ") t
      in aux "" l 
```
linea 164
```
| TmTuple l ->
    let rec aux str=function
      | [] -> "{" ^ str ^ "}"
      | [h] -> aux (str ^ string_of_term h) []
      | h::t -> aux (str ^ string_of_term h ^ ", ") t
    in aux "" l
  | TmTProj (t, idx) ->
      (match t with
        TmTuple l -> string_of_term (List.nth l idx)
        | _ -> string_of_term t)
```
linea 349
```
| TmTuple t1 -> 
    let rec axu res= function
      | [] -> TyTuple (List.rev res)
      | h::t -> axu (typeof typesCtx termsCtx h::res) t
    in axu [] t1
| TmTProj (t, idx) ->
      (match t with
        | TmTuple l -> typeof typesCtx termsCtx (List.nth l idx)
        | TmVar y -> (try 
                      match (getbinding typesCtx y) with
                        TyTuple l -> List.nth l idx
                        | _ -> raise (Type_error "Incompatible types") 
                    with
                      _ -> raise (Type_error ("no binding type for variable " ^ y)))
        | _ -> raise (Type_error "Projecting from not project type"))
```
linea 470
```
| TmTuple (t1) ->
    let rec aux res = function
      | [] -> res
      | h::t -> aux (lunion (free_vars h) res) t
    in aux [] t1
  | TmTProj (t1, idx) ->
    (match t1 with
      | TmTuple l -> free_vars (List.nth l idx)
      | _ -> free_vars t1)
```
linea 558
```
| TmTuple t ->
      TmTuple t
| TmTProj (t, id) ->
    (match t with
      TmVar y -> if y = x
                  then TmTProj(s, id)
                  else failwith "Let in without TmVar"
      | _ -> failwith "Didn't match TmVar")
```
linea 600
```
| TmTuple l -> let rec axu = function
                    | [] -> true
                    | h::t -> if isval h
                                then axu t
                                else false
                  in axu l
```
linea 770
```
| TmTProj (t1, idx) ->
    let t1' = eval1 termsCtx typesCtx t1 in
    TmTProj(t1', idx)
    (* E-Tuple *)
  | TmTuple (t1) when not (isval (TmTuple t1))-> 
    let rec axu res = function
      | [] -> TmTuple (List.rev res)
      | h::t -> if isval h
                  then axu (h::res) t
                  else axu (eval1 termsCtx typesCtx h::res) t
    in axu [] t1
```

### **2.5 Incorporacion de los registros**

#### Modulo parser.mly
linea 78:
```
| term DOT IDV
            { TmRProj ($1, $3)}
```
linea 91:
```
| IDV EQ term reg
    { TmReg ([($1,$3)] @ $4) }
```
linea 100:
```
reg:
  | COMA IDV EQ term reg
    { [($2,$4)] @ $5 }
  | /**/
    { [] }
```
#### Modulo lambda.mli
linea 9:
```
| TyReg of (string * ty) list
```
linea 38:
```
  | TmReg of (string * term) list
  | TmRProj of term * string
```
#### Modulo lambda.ml
linea 11:
```
  | TyReg of (string * ty) list
```
linea 38:
```
| TmTProj of term * int
| TmReg of (string * term) lis
```
linea 103:
```
| TyReg l ->
    let rec aux srt = function
      | [] -> "{" ^ srt ^ "}"
      | [(key, t)] -> aux (srt ^ key ^ ":" ^ string_of_ty t) []
      | (key, t1)::t -> aux (srt ^ key ^ ":" ^ string_of_ty t1 ^ ", ") t
    in aux "" l
```
linea 174:
```
| TmReg l ->
      let rec aux srt = function
        | [] -> "{" ^ srt ^ "}"
        | [(key, t1)] -> aux (srt ^ key ^ "=" ^ string_of_term t1) []
        | (key, t1)::t -> aux (srt ^ key ^ "=" ^ string_of_term t1 ^ ", ") t
      in aux "" l
  | TmRProj (t, et) ->
      (match t with
        | TmReg l -> string_of_term (List.assoc et l)
        | _ -> string_of_term t)
```
linea 368:
```
| TmReg l ->
    let rec aux res = function
      | [] -> TyReg (List.rev res)
      | (key, t1)::t -> let t1' = typeof typesCtx termsCtx t1
                        in aux ((key, t1')::res) t
    in aux [] l

    (* T-Record-Proj *)
  | TmRProj (t, et) ->
      (match t with
        | TmReg l -> typeof  typesCtx termsCtx (List.assoc et l)
        | TmVar y -> (try
                        match (getbinding typesCtx y) with
                          TyReg l -> List.assoc et l
                          | _ -> raise (Type_error "incompatible types")
                      with
                        _ -> raise (Type_error ("no binding type for variable " ^ y))) 
        | _ -> raise (Type_error "Projecting from not project type"))
```
linea 479
```
| TmReg l ->
    let rec aux res = function
      | [] -> res
      | (key, t1)::t -> aux (lunion (free_vars t1) res) t
    in aux [] l
  | TmRProj (t, key) ->
    (match t with
      | TmReg l -> free_vars (List.assoc key l)
      | _ -> free_vars t)
```
linea 566:
```
| TmReg l ->
      TmReg l
  | TmRProj (t, key) ->
      (match t with
        TmVar y -> if y = x
                      then TmRProj(s, key)
                      else failwith "Let in without TmVar"
        | _ -> failwith "Didn't match TmVar")
```
linea 606:
```
| TmReg l -> let rec axu = function
                  | [] -> true
                  | (key,t1)::t -> if isval t1
                                    then axu t
                                    else false
                in axu l 
```
linea 782
```
| TmRProj (TmReg l, key) when isval (TmReg l)->
      List.assoc key l
  
  | TmRProj (t1, key) ->
      let t1' = eval1 termsCtx typesCtx t1 in
      TmRProj (t1', key)

  | TmReg l when not (isval (TmReg l)) ->
      let rec aux res= function
        | [] -> TmReg (List.rev res)
        | (key, t1)::t -> if isval t1
                            then aux ((key,t1)::res) t
                            else let t1' = eval1 termsCtx typesCtx t1
                              in aux ((key,t1')::res) t
      in aux [] l 
```