open Core
open Ast_types

(* This module defines the type for the abstract syntax tree (AST) produced by
   parsing mlsus's source code. *)

type core_type =
  | Type_var of Type_var_name.t
  | Type_arrow of core_type * core_type
  | Type_tuple of core_type list
  | Type_constr of core_type list * Type_name.t
[@@deriving sexp_of]

type core_scheme =
  { scheme_quantifiers : Type_var_name.t list
  ; scheme_body : core_type
  }
[@@deriving sexp_of]

type pattern =
  | Pat_any
  | Pat_var of Var_name.t
  | Pat_alias of pattern * Var_name.t
  | Pat_const of constant
  | Pat_tuple of pattern list
  | Pat_constr of Constructor_name.t * pattern option
  | Pat_record of (Label_name.t * pattern) list
  | Pat_annot of pattern * core_type
[@@deriving sexp_of]

type expression =
  | Exp_var of Var_name.t
  | Exp_const of constant
  | Exp_fun of pattern * expression
  | Exp_app of expression * expression
  | Exp_let of value_binding * expression
  | Exp_exists of Type_var_name.t list * expression
  | Exp_annot of expression * core_type
  | Exp_constr of Constructor_name.t * expression option
  | Exp_record of (Label_name.t * expression) list
  | Exp_field of expression * Label_name.t
  | Exp_tuple of expression list
  | Exp_match of expression * case list
  | Exp_if_then_else of expression * expression * expression
  | Exp_sequence of expression * expression
[@@deriving sexp_of]

and value_binding =
  { value_binding_pat : pattern
  ; value_binding_exp : expression
  }
[@@deriving sexp_of]

and case =
  { case_lhs : pattern
  ; case_rhs : expression
  }
[@@deriving sexp_of]

type value_description =
  { value_name : Var_name.t
  ; value_type : core_scheme
  }
[@@deriving sexp_of]

type type_declaration =
  { type_decl_name : Type_name.t
  ; type_decl_params : Type_var_name.t list
  ; type_decl_kind : type_decl_kind
  }
[@@deriving sexp_of]

and type_decl_kind =
  | Type_decl_variant of constructor_declaration list
  | Type_decl_record of label_declaration list
  | Type_decl_abstract

and label_declaration =
  { label_name : Label_name.t
  ; label_arg : core_type
  }
[@@deriving sexp_of]

and constructor_declaration =
  { constructor_name : Constructor_name.t
  ; constructor_arg : core_type option
  }
[@@deriving sexp_of]

type structure_item =
  | Str_value of value_binding
  | Str_primitive of value_description
  | Str_type of type_declaration list
[@@deriving sexp_of]

type structure = structure_item list [@@deriving sexp_of]