
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyVar of string
  | TyTuple of ty list
  | TyReg of (string * ty) list
  | TyList of ty
  | TyVariant of (string * ty) list
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
  | TmCapitalize of term
  | TmEmptyList of ty
  | TmList of ty * term * term
  | TmIsNil of ty * term
  | TmHeadList of ty * term
  | TmTailList of ty * term
  | TmVariant of string * term * ty

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
exception Eval_failure of string;;
val eval : contextTerm -> context -> term -> term;;




