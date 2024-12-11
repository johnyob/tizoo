open Core

module type S = Tizoo_unifier.Structure.S

module Former = struct
  type 'a t =
    | Arrow of 'a * 'a
    | Constr of 'a list * string
  [@@deriving sexp]

  type 'a ctx = unit

  exception Cannot_merge

  let iter t ~f =
    match t with
    | Arrow (t1, t2) ->
      f t1;
      f t2
    | Constr (ts, _) -> List.iter ts ~f
  ;;

  let hmap t ~f =
    match t with
    | Arrow (t1, t2) -> Arrow (f t1, f t2)
    | Constr (ts, constr) -> Constr (List.map ts ~f, constr)
  ;;

  let fold t ~f ~init =
    match t with
    | Arrow (t1, t2) -> f t2 (f t1 init)
    | Constr (ts, _) -> List.fold_right ts ~f ~init
  ;;

  let merge ~ctx:_ ~create:_ ~unify _ _ t1 t2 =
    match t1, t2 with
    | Arrow (t11, t12), Arrow (t21, t22) ->
      unify t11 t21;
      unify t12 t22;
      t1
    | Constr (ts1, constr1), Constr (ts2, constr2) when String.(constr1 = constr2) ->
      (match List.iter2 ts1 ts2 ~f:unify with
       | Ok () -> t1
       | Unequal_lengths -> raise Cannot_merge)
    | _ -> raise Cannot_merge
  ;;
end
