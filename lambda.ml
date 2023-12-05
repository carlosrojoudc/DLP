
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyVar of string
  | TyTuple of ty list
  | TyReg of (string * ty) list
;;

type context =
  (string * ty) list
;;


type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term  
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmDef of string * term
  | TmTyDef of string * ty
  | TmTuple of term list
  | TmTProj of term * int
  | TmReg of (string * term) list
  | TmRProj of term * string
  | TmVarType of string


;;

type contextTerm =
  (string * term) list
;;



(* CONTEXT MANAGEMENT *)

let emptyctxTerms = 
  []
;;

let addbindingTerms ctx x bind = 
  (x, bind):: ctx
;;

let getbindingTerms ctx x = 
  List.assoc x ctx
;;

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
      "String"
  | TyVar t -> t
  | TyTuple l ->
      let rec aux str=function
        | [] -> "{" ^ str ^ "}"
        | [h] -> aux (str ^ string_of_ty h) []
        | h::t -> aux (str ^ string_of_ty h ^ ", ") t
      in aux "" l 
  | TyReg l ->
    let rec aux srt = function
      | [] -> "{" ^ srt ^ "}"
      | [(key, t)] -> aux (srt ^ key ^ ":" ^ string_of_ty t) []
      | (key, t1)::t -> aux (srt ^ key ^ ":" ^ string_of_ty t1 ^ ", ") t
    in aux "" l
;;

exception Type_error of string
;;
let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
  	  "(fix " ^ string_of_term t ^ ")"
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      "concat " ^ "(" ^ string_of_term t1 ^ ")" ^ " " ^ "(" ^ string_of_term t2 ^ ")"
  | TmDef (t1, t2) ->
      t1 ^ " = " ^ string_of_term t2
  | TmTyDef (t1, t2) ->
      t1 ^ " = " ^ string_of_ty t2
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
  | TmVarType s -> s
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec typeof typesCtx termsCtx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof typesCtx termsCtx t1 = TyBool then
        let tyT2 = typeof typesCtx termsCtx t2 in
        if typeof typesCtx termsCtx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof typesCtx termsCtx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof typesCtx termsCtx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof typesCtx termsCtx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      print_endline ("GGGGGGGGGGGGGg");
      print_endline (x);

      (try getbinding typesCtx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))
  (*| TmVar x ->
      (try getbinding ctx x with
       _ -> try getbindingTerms global x with 
       _ -> raise (Type_error ("no binding type for variable " ^ x)))*)
    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let typesCtx' = addbinding typesCtx x tyT1 in
      let termsCtx' = addbindingTerms termsCtx x t2 in
      let tyT2 = typeof typesCtx' termsCtx' t2 in
      
      let tyT1' = 
      (match tyT1 with
        | TyVar t -> typeof typesCtx' termsCtx' ((getbindingTerms termsCtx (string_of_ty(tyT1))) )
        | _ -> tyT1) in
      TyArr (tyT1', tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof typesCtx termsCtx t1 in
      let tyT2 = typeof typesCtx termsCtx t2 in

      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let termsCtx' = addbindingTerms termsCtx x t1 in 
      let tyT1 = typeof typesCtx termsCtx t1 in
      let ctx' = addbinding typesCtx x tyT1 in
      typeof ctx' termsCtx' t2
      
    (* T-Fix *)
  | TmFix t1 ->
   		let tyT1 = typeof typesCtx termsCtx t1 in
   		(match tyT1 with
   			TyArr (tyT11, tyT12) ->
   				if tyT11 = tyT12 then tyT12
   				else raise (Type_error "result of body not compatible with domain")
   		| 	_ -> raise (Type_error "arrow type expected"))

    (* new rule for string *)
  | TmString _->
      TyString
    
    (* new rule for string *)
  | TmConcat (t1, t2) ->
      if typeof typesCtx termsCtx t1 = TyString && typeof typesCtx termsCtx t2 = TyString then TyString
      else raise (Type_error "argument of concat is not a string")

  | TmDef (x, t1) ->
      let tyT1 = typeof typesCtx termsCtx t1 in
      let ctx' = addbinding typesCtx x tyT1 in
      let termsCtx' = addbindingTerms termsCtx x t1 in
      typeof ctx' termsCtx' t1
  
      (*(match tyT1 with 
        TyVar t -> print_endline ("AUUUUUU2222222222222222222222UUU");(try getbinding typesCtx x with  _ -> raise (Type_error ("no binding type for variable " ^ x)))
        | TyArr (TyVar t, TyVar t2) -> TyArr (TyVar t, TyVar t2) -> 
        | _ -> tyT1)*)

  | TmTyDef (x,tyT1) ->
      print_endline ("AUUUUUUUUU");
      (match tyT1 with
        | TyVar t -> typeof typesCtx termsCtx (TmVar t)
        | TyArr (TyVar t1,TyVar t2) -> TyArr (typeof typesCtx termsCtx (TmVar t1), typeof typesCtx termsCtx (TmVar t2))
        | TyArr (t1,TyVar t2) -> TyArr (t1, typeof typesCtx termsCtx (TmVar t2))
        | TyArr (TyVar t1,t2) -> TyArr (typeof typesCtx termsCtx (TmVar t1), t2)
        | _ -> tyT1)
      (*(match tyT1 with 
        TyVar t -> print_endline ("AUUUUUU2222222222222222222222UUU");(try getbinding typesCtx x with  _ -> raise (Type_error ("no binding type for variable " ^ x)))
        | TyArr (TyVar t, TyVar t2) -> TyArr (TyVar t, TyVar t2) -> 
        | _ -> tyT1)*)
    (* T-Tuple *)
  | TmTuple t1 -> 
    let rec axu res= function
      | [] -> TyTuple (List.rev res)
      | h::t -> axu (typeof typesCtx termsCtx h::res) t
    in axu [] t1

    (* T-Tuple-Proj *)
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
    
    (* T-Record *)
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
                          | _ -> raise (Type_error "Incompatible types")
                      with
                        _ -> raise (Type_error ("no binding type for variable " ^ y))) 
        | _ -> raise (Type_error "Projecting from not project type"))
    (* T-Var *)
  | TmVarType x ->
      print_endline ("GGGGGGGGGGGGGg");
      print_endline (x);

      (try getbinding typesCtx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))
  
      
    
;;


(* TERMS MANAGEMENT (EVALUATION) *)


let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) -> (* let x = 3 in x*)
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
  	  free_vars t
  | TmString _ ->
    []
  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmDef (t1, t2) ->
    (ldif (free_vars t2) [t1]) (*----------------------------------------------------------*)
  | TmTyDef (t1,t2) -> []
  | TmTuple (t1) ->
    let rec aux res = function
      | [] -> res
      | h::t -> aux (lunion (free_vars h) res) t
    in aux [] t1
  | TmTProj (t1, idx) ->
    (match t1 with
      | TmTuple l -> free_vars (List.nth l idx)
      | _ -> free_vars t1)
  | TmReg l ->
    let rec aux res = function
      | [] -> res
      | (key, t1)::t -> aux (lunion (free_vars t1) res) t
    in aux [] l
  | TmRProj (t, key) ->
    (match t with
      | TmReg l -> free_vars (List.assoc key l)
      | _ -> free_vars t)
  | TmVarType s ->
      [s]
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm termsCtx typesCtx = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1 termsCtx typesCtx, subst x s t2 termsCtx typesCtx, subst x s t3 termsCtx typesCtx)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t termsCtx typesCtx)
  | TmPred t ->
      TmPred (subst x s t termsCtx typesCtx)
  | TmIsZero t ->
      TmIsZero (subst x s t termsCtx typesCtx)
  | TmVar y ->
      print_endline "asDSADSADA";
      print_endline (string_of_term tm);
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t termsCtx typesCtx)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t termsCtx typesCtx) termsCtx typesCtx)
  | TmApp (t1, t2) ->
      TmApp (subst x s t1 termsCtx typesCtx, subst x s t2 termsCtx typesCtx)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1 termsCtx typesCtx, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1 termsCtx typesCtx, subst x s t2 termsCtx typesCtx)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1 termsCtx typesCtx, subst x s (subst y (TmVar z) t2 termsCtx typesCtx) termsCtx typesCtx)
  | TmFix t ->
  	  TmFix (subst x s t termsCtx typesCtx)
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1 termsCtx typesCtx, subst x s t2 termsCtx typesCtx)
  | TmDef (t1, t2) ->
      t2
  | TmTyDef (t1, t2) ->
      tm
  | TmTuple t ->
      TmTuple t
  | TmTProj (t, id) ->
      (match t with
        TmVar y -> if y = x
                    then TmTProj(s, id)
                    else failwith "Let in without TmVar"
        | _ -> failwith "Didn't match TmVar")
  | TmReg l ->
      TmReg l
  | TmRProj (t, key) ->
      (match t with
        TmVar y -> if y = x
                      then TmRProj(s, key)
                      else failwith "Let in without TmVar"
        | _ -> failwith "Didn't match TmVar")
  | TmVarType y ->
      raise (Type_error "Cant apply to a type")
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | t when isnumericval t -> true
  | TmTuple l -> let rec axu = function
                    | [] -> true
                    | h::t -> if isval h
                                then axu t
                                else false
                  in axu l
  | TmReg l -> let rec axu = function
                  | [] -> true
                  | (key,t1)::t -> if isval t1
                                    then axu t
                                    else false
                in axu l 
  | _ -> false
;;

exception NoRuleApplies
;;

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


let rec eval1 termsCtx typesCtx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2
      

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 termsCtx typesCtx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 termsCtx typesCtx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 termsCtx typesCtx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 termsCtx typesCtx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12 termsCtx typesCtx (*Substituye x por v2 en t12*)
      (*EJEMPLO:
      x = 5;;
      f = lambda y : Nat. x;;
      f 2;;

      Al aplicar f 2... =>
      y = x
      Nat = _
      x = t12
      v2 = 2
      *)

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 termsCtx typesCtx t2 in
      TmApp (v1, t2')
  
    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 termsCtx typesCtx t1 in
      TmApp (t1', t2)


    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      print_endline("ASAASSAAS");
      print_endline(string_of_term t2);
      subst x v1 t2 termsCtx typesCtx

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      print_endline("CCCCCCCCCCCCCCCCAAS");

      let t1' = eval1 termsCtx typesCtx t1 in
      TmLetIn (x, t1', t2)
  
  	(* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
  	  subst x tm t2 termsCtx typesCtx

  
	  (* E-Fix *)
  | TmFix t1 ->
  	  let t1' = eval1 termsCtx typesCtx t1 in
  	  TmFix t1'

  | TmVar y ->
    getbindingTerms termsCtx y
  
    (* new rule for string*)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
    
    (* new rule for string*)
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 termsCtx typesCtx t2 in
      TmConcat (TmString s1, t2')
    
    (* new rule for string*)
  | TmConcat (t1, s2) ->
      let t1' = eval1 termsCtx typesCtx t1 in
      TmConcat (t1', s2)


  | TmDef (x, t1) ->
    if esAbstraccion termsCtx t1 then devolverAbstraccion termsCtx typesCtx t1 else t1
    (*let t1' = eval1 termsCtx typesCtx t1 in TmDef (x,t1')*)
    (*(match t1 with
      | TmAbs (_,_,_) -> let t1' = eval1 termsCtx typesCtx t1 in TmDef (x,t1')
      | _ -> t1)*)

    (* E-ProjTuple *)
  | TmTProj (TmTuple l, idx) when isval (TmTuple l)->
    List.nth l idx

    (* E-Proj *)
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
  | TmVarType t -> raise (Type_error "Cant apply to a type")
  | _ -> print_endline("ZZZZZZZZZZZZZZZZZZzz");
      raise NoRuleApplies
;;

let rec eval termsCtx typesCtx tm =
  try
    (*print_endline("TERMINO EVAL: " ^ string_of_term(tm));*)
    let tm' = eval1 termsCtx typesCtx tm 
    in eval termsCtx typesCtx tm'
  with
    NoRuleApplies -> tm
;;

