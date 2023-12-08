open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*Reads a command until it finds ";;"*)
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

(*Used to check if term is a TmDef or TmTyDef definition*)
let isDefinition = function
  | TmDef (_,_) -> true
  | TmTyDef (_,_) -> true
  | _ -> false

(*Used to check if is a global term or type definition*)
let comienza_con_mayuscula (cadena : string) : bool =
  let patron = Str.regexp "^[A-Z]" in
  try
    ignore (Str.search_forward patron cadena 0);
    true
  with Not_found -> false

  
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop typesCtx termsCtx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_command ())) in
      if isDefinition tm (*TmDef or TmTyDef*)

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

    with
      Lexical_error ->
          print_endline "lexical error";
          loop typesCtx termsCtx
      | Parse_error ->
          print_endline "syntax error";
          loop typesCtx termsCtx
      | Type_error e ->
          print_endline ("type error: " ^ e);
          loop typesCtx termsCtx
      | End_of_file ->
          print_endline "...bye!!!";
      | Not_found ->
          print_endline "Otro error";
          loop typesCtx termsCtx
      | Eval_failure e ->
          print_endline ("eval error: " ^ e);
          loop typesCtx termsCtx

  in
    loop emptyctx emptyctxTerms
  ;;

top_level_loop ()
;;

