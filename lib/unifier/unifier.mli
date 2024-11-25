module Make (S : Structure.S) : sig
  module Type : sig
    (** [t] represents a type *)
    type t

    (** [make structure] creates a new unification type w/ structure [structure]. *)
    val create : t S.t -> t

    (** [structure t] returns the structure of [t]. *)
    val structure : t -> t S.t

    (** [set_structure t structure] sets the structure of [t] to [structure]. *)
    val set_structure : t -> t S.t -> unit
  end

  (** [unify ~ctx t1 t2] equates the types [t1] and [t2].

      [Unify (t1, t2)] is raised if the two node cannot be unified. *)

  exception Unify of Type.t * Type.t

  val unify : ctx:Type.t S.ctx -> Type.t -> Type.t -> unit
end
