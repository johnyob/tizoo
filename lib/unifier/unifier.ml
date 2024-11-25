open Tizoo_std

module Make (Structure : Structure.S) = struct
  module Type = struct
    type t = desc Union_find.t
    and desc = { structure : t Structure.t }

    (* [create structure] returns a fresh type w/ structure [structure]. *)
    let create structure = Union_find.create { structure }

    (* [structure t] returns the structure of [t]. *)
    let structure t = (Union_find.get t).structure

    (* [set_structure t structure] sets the structure of [t] to [structure]. *)
    let set_structure t structure = Union_find.set t { structure }
  end

  open Type

  let rec unify_exn ~ctx t1 t2 = Union_find.union ~f:(unify_desc ~ctx) t1 t2

  and unify_desc ~ctx desc1 desc2 =
    { structure = unify_structure ~ctx desc1.structure desc2.structure }

  and unify_structure ~ctx structure1 structure2 =
    Structure.merge ~ctx ~create:Type.create ~unify:(unify_exn ~ctx) structure1 structure2
  ;;

  exception Unify of Type.t * Type.t

  let unify ~ctx t1 t2 =
    try unify_exn ~ctx t1 t2 with
    | Structure.Cannot_merge -> raise (Unify (t1, t2))
  ;;
end
