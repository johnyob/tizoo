open! Import

module Type = struct
  module Var = Var.Make (struct
      let module_name = "Type.Var"
    end)

  type t =
    | Structure of t Structure.Former.t
    | Var of Var.t
  [@@deriving sexp]

  let var v = Var v
  let ( @-> ) t1 t2 = Structure (Arrow (t1, t2))
  let constr ts constr = Structure (Constr (ts, constr))
end

module Var = Var.Make (struct
    let module_name = "Constraint.Var"
  end)

module Scruintee = struct
  type t =
    | Arrow of Type.Var.t * Type.Var.t
    | Constr of Type.Var.t list * string
  [@@deriving sexp]
end

module Type_scheme = struct
  type 'a t =
    { type_vars : Type.Var.t list
    ; in_ : 'a
    ; type_ : Type.t
    }
  [@@deriving sexp]
end

module Closure = struct
  type t = { type_vars : Type.Var.Set.t } [@@unboxed] [@@deriving sexp]

  let of_list type_vars = { type_vars = Type.Var.Set.of_list type_vars }
end

type t =
  | True
  | False
  | Conj of t * t
  | Eq of Type.t * Type.t
  | Exists of Type.Var.t * t
  | Match of Type.Var.t * Closure.t * (Scruintee.t -> t)
  | Let of Var.t * t Type_scheme.t * t
  | Instance of Var.t * Type.t
[@@deriving sexp]

let tt = True
let ff = False
let ( &~ ) t1 t2 = Conj (t1, t2)
let ( =~ ) type1 type2 = Eq (type1, type2)
let exists type_var t = Exists (type_var, t)
let ( #= ) x scheme = x, scheme
let mono_scheme type_ : t Type_scheme.t = { type_vars = []; in_ = tt; type_ }
let ( @=> ) t1 t2 = t1, t2
let ( @. ) t1 t2 = t1, t2
let poly_scheme (type_vars, (in_, type_)) : t Type_scheme.t = { type_vars; in_; type_ }
let let_ (x, scheme) ~in_ = Let (x, scheme, in_)
let inst x type_ = Instance (x, type_)
let match_ type_var ~closure ~with_ = Match (type_var, Closure.of_list closure, with_)
