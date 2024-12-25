open Core
open Mlsus_ast.Ast_types
open Mlsus_std

module Type = struct
  module Var = Var.Make (struct
      let module_name = "Type.Var"
    end)

  type t =
    | Arrow of t * t
    | Tuple of t list
    | Constr of t list * Constructor_name.t
    | Var of Var.t
  [@@deriving sexp]

  let var v = Var v
  let ( @-> ) t1 t2 = Arrow (t1, t2)
  let constr ts constr = Constr (ts, constr)
  let tuple ts = Tuple ts
end

module Var = Var.Make (struct
    let module_name = "Constraint.Var"
  end)

type t =
  | True
  | False
  | Conj of t * t
  | Eq of Type.t * Type.t
  | Exists of Type.Var.t * t
  | Let of Var.t * scheme * t
  | Instance of Var.t * Type.t

and scheme =
  { type_vars : Type.Var.t list
  ; in_ : t
  ; type_ : Type.t
  }
[@@deriving sexp]

let tt = True
let ff = False
let ( &~ ) t1 t2 = Conj (t1, t2)
let ( =~ ) type1 type2 = Eq (type1, type2)
let exists type_var t = Exists (type_var, t)
let ( #= ) x scheme = x, scheme
let mono_scheme type_ = { type_vars = []; in_ = tt; type_ }
let ( @=> ) t1 t2 = t1, t2
let ( @. ) t1 t2 = t1, t2
let poly_scheme (type_vars, (in_, type_)) = { type_vars; in_; type_ }
let let_ (x, scheme) ~in_ = Let (x, scheme, in_)
let inst x type_ = Instance (x, type_)
