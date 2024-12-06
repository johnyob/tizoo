open! Import
module F = Structure.Former

module Level = Region_tree.Level

module Region = struct
  (** The [Generalization] module manages the generalisation of graphical types.
      Each type belongs to a 'region', which indicates where those types are
      existentially bound in the solver's stack. *)
  type 'a t = { mutable types : 'a list } [@@deriving sexp_of]
end

module Region_tree = Region_tree.Make (Region)

module rec Status : sig
  type t =
    | Instance of U.Type.t Region_tree.node
    | Partial_generic of
        { region_node : U.Type.t Region_tree.node
        ; instances : U.Type.t list
        }
    | Generic
  [@@deriving sexp_of]
end =
  Status

and Suspended_var : sig
  type 'a t =
    | Empty
    | Empty_one_or_more_handlers of 'a handler list
  [@@deriving sexp_of]

  and 'a handler =
    { closure : 'a list
    ; run : 'a F.t -> unit
    }
  [@@deriving sexp_of]
end =
  Suspended_var

and Suspended_first_order : sig
  type 'a t =
    | Var of 'a Suspended_var.t
    | Structure of 'a F.t
  [@@deriving sexp_of]
end =
  Suspended_first_order

and S : sig
  type 'a t =
    { id : Identifier.t
    ; inner : 'a Suspended_first_order.t
    ; status : Status.t
    }
  [@@deriving sexp_of]
end =
  S

and U : (Unifier.S with type 'a structure := 'a S.t) = Unifier.Make (S)

module Type = U.Type
