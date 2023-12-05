
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyVar of string
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
  | TmTuple of term list
  | TmProj of term * int
  | TmDef of string * term
  | TmTyBool
  | TmTyNat
  | TmTyArr of term * term
  | TmTyString
  
  (*| TmDefTy of string * ty*) 

;;

type contextTerm =
  (string * term) list
;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> context;;
val getbinding : context -> string -> ty;;

val emptyctxTerms : contextTerm;;
val addbindingTerms : contextTerm -> string -> term -> contextTerm;;
val getbindingTerms : contextTerm -> string -> term;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> contextTerm -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : contextTerm -> context -> term -> term;;



