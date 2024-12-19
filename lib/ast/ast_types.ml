open Core

(* This module defines auxiliary AST types used by [Ast] types. *)

type constant =
  | Const_int of int
  | Const_float of float
  | Const_bool of bool
  | Const_char of char
  | Const_string of string
  | Const_unit
[@@deriving sexp_of]

module Ident = struct
  module type S = sig
    type t = private string

    val create : string -> t

    include Identifiable.S with type t := t
  end

  module Make () : S = struct
    include String

    let create s = s
  end
end

module Var_name = Ident.Make ()
module Type_var_name = Ident.Make ()
module Type_name = Ident.Make ()
module Constructor_name = Ident.Make ()
module Label_name = Ident.Make ()
