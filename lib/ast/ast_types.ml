open Core
open Grace

(* This module defines auxiliary AST types used by [Ast] types. *)

type constant =
  | Const_int of int
  | Const_float of float
  | Const_bool of bool
  | Const_char of char
  | Const_string of string
  | Const_unit
[@@deriving sexp_of]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving sexp_of]

type 'a with_range =
  { it : 'a
  ; range : Range.t
  }
[@@deriving sexp_of]

type type_variable_name = Type_var_name of string with_range
[@@unboxed] [@@deriving sexp_of]

type constructor_name = Constructor_name of string with_range
[@@unboxed] [@@deriving sexp_of]

type label_name = Label_name of string with_range [@@unboxed] [@@deriving sexp_of]
type variable_name = Var_name of string with_range [@@unboxed] [@@deriving sexp_of]
type type_name = Type_name of string with_range [@@unboxed] [@@deriving sexp_of]
