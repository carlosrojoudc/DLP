
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"	  { LETREC }
  | "in"        { IN }
  | "concat"    { CONCAT }
  | "capitalize"{ CAPITALIZE}
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "List"      { LIST }
  | "nil"       { NILLIST}
  | "cons"      { CONSLIST }
  | "isnil"     { ISNILLIST}
  | "head"      { HEADLIST}
  | "tail"      { TAILLIST}
  | "fix"       { FIX }
  | "as"        { AS }
  | "case"      { CASE }
  | "of"        { OF }
  | "=>"        { BIGARROW}
  | '<'         { LTAG } 
  | '>'         { RTAG }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LCORCH }
  | '}'         { RCORCH }
  | '['         { LBRACK }
  | ']'         { RBRACK }
  | ','         { COMA }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['a'-'z' '_' '0'-'9']*
                { IDT (Lexing.lexeme lexbuf) }      
  | '"'[^ '"' ';' '\n']* '"'
                { let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s - 2))}
  | eof         { EOF }
  | _           { raise Lexical_error }

