open! Import
open Ast_types
module Type_ident = Constraint.Type.Ident

module Constructor_ident = Var.Make (struct
    let module_name = "Adt.Constructor_name"
  end)

type type_expr =
  | Type_var of Type_var_name.t
  | Type_arrow of type_expr * type_expr
  | Type_tuple of type_expr list
  | Type_constr of type_expr list * Type_ident.t
[@@deriving sexp_of]

(** Type definitions to encode user-defined types e.g. variants. *)

type type_declaration =
  { type_name : Type_name.t (** The user-defined name of the type that is declared. *)
  ; type_ident : Type_ident.t (** The unique name of the type. *)
  ; type_kind : type_decl_kind (** The kind of the type declaration. *)
  }
[@@deriving sexp_of]

and type_decl_kind =
  | Type_variant of constructor_declaration list
  | Type_abstract
[@@deriving sexp_of]

and constructor_declaration =
  { constructor_name : Constructor_name.t (** The user-defined name of the constructor. *)
  ; constructor_ident : Constructor_ident.t (** The unique name of the constructor. *)
  ; constructor_alphas : Type_var_name.t list
  ; constructor_arg : type_expr option
  ; constructor_type : type_expr
  ; constructor_type_ident : Type_ident.t
  }

and constructor_arity =
  | Zero
  | One
[@@deriving sexp_of]
