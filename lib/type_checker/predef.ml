open! Import
open Constraint
open Ast_types

(* Pre-defined types have their own [id_source] since the type names cannot be parsed => no conflict possible *)
let id_source = Identifier.create_source ()
let int_ident = Type.Ident.create ~id_source ~name:"Stdlib.int" ()
let bool_ident = Type.Ident.create ~id_source ~name:"Stdlib.bool" ()
let unit_ident = Type.Ident.create ~id_source ~name:"Stdlib.unit" ()
let int = Type.(constr [] int_ident)
let bool = Type.(constr [] bool_ident)
let unit = Type.(constr [] unit_ident)

module Env = struct
  let bool_bop = Type.(bool @-> bool @-> bool)
  let bool_uop = Type.(bool @-> bool)
  let int_bop = Type.(int @-> int @-> int)
  let int_uop = Type.(int @-> int)
  let int_comparator = Type.(int @-> int @-> bool)

  let type_decl name ident =
    { Adt.type_name = Type_name.create name
    ; type_ident = ident
    ; type_kind = Type_abstract
    }
  ;;

  let t = [ "int", int_ident; "bool", bool_ident; "unit", unit_ident ]

  let v =
    [ "( || )", bool_bop
    ; "( && )", bool_bop
    ; "not", bool_uop
    ; "( = )", int_comparator
    ; "( <> )", int_comparator
    ; "( < )", int_comparator
    ; "( > )", int_comparator
    ; "( <= )", int_comparator
    ; "( >= )", int_comparator
    ; "( + )", int_bop
    ; "( - )", int_bop
    ; "( * )", int_bop
    ; "( / )", int_bop
    ; "unary( - )", int_uop
    ]
  ;;

  let wrap k =
    let open Or_error.Let_syntax in
    let env = Env.empty () in
    let env =
      List.fold t ~init:env ~f:(fun env (type_str, type_ident) ->
        Env.add_type_decl env (type_decl type_str type_ident))
    in
    let env, bindings =
      List.fold_map v ~init:env ~f:(fun env (var_str, type_) ->
        let var = Var_name.create var_str in
        let cvar = Var.create ~id_source:env.id_source ~name:var_str () in
        let env = Env.add_var env ~var ~cvar in
        env, (cvar, type_))
    in
    let%map c = k env in
    List.fold_right bindings ~init:c ~f:(fun (var, type_) in_ ->
      let_ var #= (mono_scheme type_) ~in_)
  ;;
end
