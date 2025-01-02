open! Import
open Ast_types
open Adt

type t =
  { id_source : Identifier.source
  ; constrs : constructor_declaration Constructor_name.Map.t
  ; types : type_declaration list Type_name.Map.t
  ; type_vars : Constraint.Type.Var.t Type_var_name.Map.t
  ; vars : Constraint.Var.t Var_name.Map.t
  }

let empty () =
  { id_source = Identifier.create_source ()
  ; constrs = Constructor_name.Map.empty
  ; type_vars = Type_var_name.Map.empty
  ; types = Type_name.Map.empty
  ; vars = Var_name.Map.empty
  }
;;

let add_constr_decl t (constr_decl : constructor_declaration) =
  { t with
    constrs = Map.set t.constrs ~key:constr_decl.constructor_name ~data:constr_decl
  }
;;

let add_type_decl t type_decl =
  let t =
    { t with types = Map.add_multi t.types ~key:type_decl.type_name ~data:type_decl }
  in
  match type_decl.type_kind with
  | Type_variant constr_decls ->
    List.fold_left constr_decls ~init:t ~f:(fun t constr_decl ->
      add_constr_decl t constr_decl)
  | Type_abstract -> t
;;

let find_constr t constr = Map.find t.constrs constr
let find_type_decl t type_name = Map.find t.types type_name
let find_type_var t type_var = Map.find t.type_vars type_var
let find_var t var = Map.find t.vars var

let add_type_var t ~type_var ~ctype_var =
  { t with type_vars = Map.set t.type_vars ~key:type_var ~data:ctype_var }
;;

let add_var t ~var ~cvar = { t with vars = Map.set t.vars ~key:var ~data:cvar }
