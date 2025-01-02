open! Import
open Constraint

let infer_exp exp =
  Predef.Env.wrap
  @@ fun env ->
  let open Or_error.Let_syntax in
  let exp_type = Type.Var.create ~id_source:env.id_source ~name:"exp_type0" () in
  let%map c = Infer.Expression.infer_exp ~env exp (Type.var exp_type) in
  exists exp_type c
;;

let infer_str str = Predef.Env.wrap @@ fun env -> Infer.Structure.infer_str ~env str
