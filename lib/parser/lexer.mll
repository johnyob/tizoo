
{
open Core
open Token
 
exception Lexer_error of string

let keywords = 
  [ "let", LET 
  ; "and", AND 
  ; "in", IN 
  ; "if", IF 
  ; "then", THEN 
  ; "else", ELSE 
  ; "true", CONST_TRUE
  ; "false", CONST_FALSE
  ; "fun", FUN 
  ; "match", MATCH 
  ; "with", WITH 
  ; "exists", EXISTS 
  ; "type", TYPE 
  ; "as", AS 
  ; "of", OF 
  ; "external", EXTERNAL
  ]
;;

let find_keyword =
  let keyword_tbl = Hashtbl.create (module String) in
  List.iter keywords ~f:(fun (keyword, token) ->
      Hashtbl.set keyword_tbl ~key:keyword ~data:token);
  Hashtbl.find keyword_tbl
;;
}

let upper = ['A' - 'Z']
let lower = ['a' - 'z']
let letter = lower | upper
 
let digit = ['0' - '9']
let space = [' ' '\t']
let newline = '\r'? '\n'

let id_char = lower | digit | ['_' '\'']
let id = lower id_char*
let upper_id = upper id_char*

let sign = '-'?
let int = sign digit+

rule read = 
  parse
  (* reserved operators *)
  | "->"                          
      { RIGHT_ARROW }
  | ":"
      { COLON }
  | "="
      { EQUAL }
  | "."
      { DOT }
  | ","
      { COMMA }
  | ";;"
      { SEMI_SEMI_COLON }
  | ";"
      { SEMI_COLON }
  | "*"
      { STAR }
  | "_"
      { UNDERSCORE }
  | "|"
      { BAR }

  (* comments *)
  | "(*"
      { read_comment lexbuf }

  (* predefined operators (fixed) *)

  | "<"
      { LESS }
  | ">"
      { GREATER }
  | "<>"
      { LESS_GREATER }
  | "<="
      { LESS_EQUAL }
  | ">="
      { GREATER_EQUAL }
  | ">"
      { GREATER }
  | "||"
      { BAR_BAR }
  | "&&"
      { AND_AND }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "/"
      { SLASH }

  (* identifiers (or keywords) *)
  | id as id
      { match find_keyword id with 
        | Some token -> token 
        | None -> IDENT id }
  | upper_id as id 
      { UPPER_IDENT id }

  (* constants *)
  | "()"
      { CONST_UNIT }
  | int as n
      { CONST_INT (Int.of_string n) }
  
  | space+
      { read lexbuf }
  | newline
      { Lexing.new_line lexbuf; read lexbuf }

  | "\'"
      { QUOTE }

  (* braces *)
  | "("
      { LEFT_PAREN }
  | ")"
      { RIGHT_PAREN }

  | eof
      { EOF }
  | _ as c                            
      { raise (Lexer_error [%string "Unexpected comment %{c#Char}"]) }

(** Read a comment delimited by (* ... *)
    Nesting is not permitted. *)
and read_comment = 
  parse
  | "*)"
      { read lexbuf }
  | newline 
      { Lexing.new_line lexbuf; read_comment lexbuf }
  | eof
      { raise (Lexer_error "Unclosed comment") }
  | _
      { read_comment lexbuf }
