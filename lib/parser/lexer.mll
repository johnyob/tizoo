
{
open Core
open Token
 
exception Lexer_error of string

let char_unescape c = 
  match c with
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | 't' -> '\t'
  | c -> c
;;

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

let frac = '.' digit+
let float = sign (int? frac | int '.')

let escape_char = '\\'
let escaped_char = ['n' 't' '"' '\\' '\'' 'b' 'r']
let ascii_char = [^ '\\' '\'']

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
  | float as f 
      { CONST_FLOAT (Float.of_string f) }
  | "\""
      { read_string (Buffer.create 17) lexbuf }
  | "\'" (ascii_char as c) "\'"
      { CONST_CHAR c }
  | "\'" 
    escape_char (escaped_char as c) 
    "\'"    
      { CONST_CHAR (char_unescape c) }                         
  
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
  | "{"
      { LEFT_BRACE }
  | "}"
      { RIGHT_BRACE }

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

and read_string buf = 
  parse
  | '"'
      { CONST_STRING (Buffer.contents buf) }
  | escape_char (escaped_char as c)          
      { Buffer.add_char buf (char_unescape c); 
        read_string buf lexbuf }
  | [^ '"' '\\']+ as s                             
      { Buffer.add_string buf s; 
        read_string buf lexbuf }
  | _                                       
      { raise (Lexer_error "Illegal character") }
  | eof                                     
      { raise (Lexer_error "String is not terminated") }