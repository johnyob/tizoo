open Tizoo_std

module type S = sig
  type 'a structure

  module Type : sig
    (** [t] represents a type *)
    type t [@@deriving sexp_of]

    (** [make structure] creates a new unification type w/ structure [structure]. *)
    val create : t structure -> t

    (** [structure t] returns the structure of [t]. *)
    val structure : t -> t structure

    (** [set_structure t structure] sets the structure of [t] to [structure]. *)
    val set_structure : t -> t structure -> unit

    (** [is_representative t] returns whether [t] is the representative of it's equivalence class. *)
    val is_representative : t -> bool
  end

  module Make_unify (M : Structure.Mergable with type 'a t := 'a structure) : sig
    (** [unify ~ctx t1 t2] equates the types [t1] and [t2].

        [Unify (t1, t2)] is raised if the two node cannot be unified. *)

    exception Unify of Type.t * Type.t

    val unify : ctx:Type.t M.ctx -> Type.t -> Type.t -> unit
  end
end

module Make (S : Structure.Basic) = struct
  module Type = struct
    type t = desc Union_find.t [@@deriving sexp_of]
    and desc = { structure : t S.t }

    let create structure = Union_find.create { structure }
    let structure t = (Union_find.get t).structure
    let set_structure t structure = Union_find.set t { structure }
    let is_representative t = Union_find.is_root t
  end

  open Type

  module Make_unify (M : Structure.Mergable with type 'a t := 'a S.t) = struct
    let rec unify_exn ~ctx t1 t2 = Union_find.union ~f:(unify_desc ~ctx t1 t1) t1 t2

    and unify_desc ~ctx t1 t2 desc1 desc2 =
      { structure = unify_structure ~ctx t1 t2 desc1.structure desc2.structure }

    and unify_structure ~ctx t1 t2 structure1 structure2 =
      M.merge ~ctx ~create:Type.create ~unify:(unify_exn ~ctx) t1 t2 structure1 structure2
    ;;

    exception Unify of Type.t * Type.t

    let unify ~ctx t1 t2 =
      try unify_exn ~ctx t1 t2 with
      | M.Cannot_merge -> raise (Unify (t1, t2))
    ;;
  end
end
