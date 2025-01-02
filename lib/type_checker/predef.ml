open! Import
open Ast_types
open Constraint

let int = Type.constr [] (Type_name.create "int")
let bool = Type.constr [] (Type_name.create "bool")
let unit = Type.constr [] (Type_name.create "unit")

module Env = struct
  let bool_bop = Type.(bool @-> bool @-> bool)
  let bool_uop = Type.(bool @-> bool)
  let int_bop = Type.(int @-> int @-> int)
  let int_uop = Type.(int @-> int)
  let int_comparator = Type.(int @-> int @-> bool)

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
