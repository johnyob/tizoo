open! Import

module type S = Mlsus_unifier.Structure.S

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

  let merge ~ctx:() ~create:_ ~unify ~type1:_ ~type2:_ t1 t2 =
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

module Suspended_first_order (S : S) = struct
  module Var = struct
    type 'a t =
      | Empty
      | Empty_one_or_more_handlers of 'a handler list

    and 'a handler = { run : 'a S.t -> unit } [@@unboxed] [@@deriving sexp_of]

    let merge t1 t2 =
      match t1, t2 with
      | Empty, Empty -> Empty
      | Empty_one_or_more_handlers handlers, Empty
      | Empty, Empty_one_or_more_handlers handlers -> Empty_one_or_more_handlers handlers
      | Empty_one_or_more_handlers handlers1, Empty_one_or_more_handlers handlers2 ->
        Empty_one_or_more_handlers (handlers1 @ handlers2)
    ;;

    let add_handler t handler =
      match t with
      | Empty -> Empty_one_or_more_handlers [ handler ]
      | Empty_one_or_more_handlers handlers ->
        Empty_one_or_more_handlers (handler :: handlers)
    ;;

    let fill t s ~schedule_handler =
      match t with
      | Empty -> ()
      | Empty_one_or_more_handlers handlers -> List.iter handlers ~f:(schedule_handler s)
    ;;
  end

  type 'a t =
    | Var of 'a Var.t
    | Structure of 'a S.t
  [@@deriving sexp_of]

  exception Cannot_merge = S.Cannot_merge

  type 'a ctx =
    { schedule_handler : 'a S.t -> 'a Var.handler -> unit
    ; super : 'a S.ctx
    }

  let iter t ~f =
    match t with
    | Var _ -> ()
    | Structure s -> S.iter s ~f
  ;;

  let fold t ~f ~init =
    match t with
    | Var _ -> init
    | Structure s -> S.fold s ~f ~init
  ;;

  let map t ~f =
    (* FIXME:
       This isn't really map since the ['a] in [Suspended_var] is
       in a negative position, making it costly to map over. *)
    match t with
    | Var svar -> Var svar
    | Structure s -> Structure (S.map s ~f)
  ;;

  let merge ~ctx ~create ~unify ~type1 ~type2 t1 t2 =
    match t1, t2 with
    | Var var1, Var var2 -> Var (Var.merge var1 var2)
    | Structure s, Var var | Var var, Structure s ->
      Var.fill var s ~schedule_handler:ctx.schedule_handler;
      Structure s
    | Structure s1, Structure s2 ->
      Structure
        (S.merge
           ~ctx:ctx.super
           ~create:(fun s -> create (Structure s))
           ~unify
           ~type1
           ~type2
           s1
           s2)
  ;;
end
