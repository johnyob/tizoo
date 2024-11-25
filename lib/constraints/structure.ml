open Core

module type S = Tizoo_unifier.Structure.S

module Ml = struct
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

  let map t ~f =
    match t with
    | Arrow (t1, t2) -> Arrow (f t1, f t2)
    | Constr (ts, constr) -> Constr (List.map ts ~f, constr)
  ;;

  let fold t ~f ~init =
    match t with
    | Arrow (t1, t2) -> f t2 (f t1 init)
    | Constr (ts, _) -> List.fold_right ts ~f ~init
  ;;

  let merge ~ctx:_ ~create:_ ~unify t1 t2 =
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

module First_order (S : S) = struct
  include S

  type 'a t =
    | Var
    | Structure of 'a S.t
  [@@deriving sexp_of]

  let iter t ~f =
    match t with
    | Var -> ()
    | Structure structure -> S.iter structure ~f
  ;;

  let map t ~f =
    match t with
    | Var -> Var
    | Structure structure -> Structure (S.map structure ~f)
  ;;

  let fold t ~f ~init =
    match t with
    | Var -> init
    | Structure structure -> S.fold structure ~f ~init
  ;;

  let merge ~ctx ~create ~unify t1 t2 =
    let create structure = create (Structure structure) in
    match t1, t2 with
    | Var, t | t, Var -> t
    | Structure structure1, Structure structure2 ->
      Structure (S.merge ~ctx ~create ~unify structure1 structure2)
  ;;
end
