open! Import
open Ast_types

type type_expr = Ast.core_type [@@deriving sexp_of]

(** Type definitions to encode user-defined types e.g. variants. *)

type type_declaration =
  { type_name : Type_name.t
  ; type_kind : type_decl_kind
  }
[@@deriving sexp_of]

and type_decl_kind =
  | Type_variant of constructor_declaration list
  | Type_abstract
[@@deriving sexp_of]

and constructor_declaration =
  { constructor_name : Constructor_name.t
  ; constructor_alphas : Type_var_name.t list
  ; constructor_arg : type_expr option
  ; constructor_type : type_expr
  }
[@@deriving sexp_of]
