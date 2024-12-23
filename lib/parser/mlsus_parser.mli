open Core
open Mlsus_ast

module Token : sig
  type t [@@deriving to_string]

  include Pretty_printer.S with type t := t

  val is_eof : t -> bool
end

module Lexer : sig
  type error = [ `Lexer_error of string ]

  val read_token : Lexing.lexbuf -> (Token.t, [> error ]) result
  val read_tokens : ?keep_eof:bool -> Lexing.lexbuf -> (Token.t list, [> error ]) result
end

module Parser : sig
  type error =
    [ Lexer.error
    | `Parser_error
    ]

  type ('a, 'err) t = Lexing.lexbuf -> ('a, ([> error ] as 'err)) result

  val parse_core_type : (Ast.core_type, 'err) t
  val parse_expression : (Ast.expression, 'err) t
  val parse_structure : (Ast.structure, 'err) t
end
