open! Import
open Types

module Scheduler0 = struct
  type handler =
    { context : context
    ; run : unit -> unit
    }

  and context = Type.t Region_tree.node

  type t =
    { handler_queue : handler Queue.t
    ; exit_queue : (Level.t, (Identifier.t, context) Hashtbl.t) Hashtbl.t
    }

  let create () =
    { handler_queue = Queue.create (); exit_queue = Hashtbl.create (module Level) }
  ;;

  let enqueue_handler t ~context ~run = Queue.enqueue t.handler_queue { context; run }
  let t_ref = ref (create ())
  let t () = !t_ref
  let reset () = t_ref := create ()
end

module Status = struct
  include Types.Status

  let region t =
    match t with
    | Instance rn -> Some rn
    | Partial_generic { region_node = rn; _ } -> Some rn
    | Generic -> None
  ;;

  let merge t1 t2 ~partial_unify =
    match t1, t2 with
    | Generic, _ | _, Generic -> assert false
    | Partial_generic { region_node = srn; instances }, Instance irn
    | Instance irn, Partial_generic { region_node = srn; instances } ->
      List.iter instances ~f:partial_unify;
      Partial_generic
        { region_node = Region_tree.nearest_common_ancestor srn irn; instances }
    | ( Partial_generic { region_node = rn1; instances = is1 }
      , Partial_generic { region_node = rn2; instances = is2 } ) ->
      let instances = is1 @ is2 in
      List.iter instances ~f:partial_unify;
      Partial_generic
        { region_node = Region_tree.nearest_common_ancestor rn1 rn2; instances }
    | Instance rn1, Instance rn2 -> Instance (Region_tree.nearest_common_ancestor rn1 rn2)
  ;;
end

module Suspended_var = struct
  include Types.Suspended_var

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

  let fill t s ~context_from_closure =
    match t with
    | Empty -> ()
    | Empty_one_or_more_handlers handlers ->
      List.iter handlers ~f:(fun handler ->
        Scheduler0.(
          enqueue_handler
            (t ())
            ~context:(context_from_closure handler.closure)
            ~run:(fun () -> handler.run s)))
  ;;
end

module Suspended_first_order = struct
  include Types.Suspended_first_order

  type 'a ctx =
    { region_node : Type.t Region_tree.node
    ; region_of : 'a -> Type.t Region_tree.node
    ; super : 'a F.ctx
    }

  exception Cannot_merge = F.Cannot_merge

  let merge ~ctx ~create ~unify t1 t2 =
    match t1, t2 with
    | Var var1, Var var2 -> Var (Suspended_var.merge var1 var2)
    | Structure s, Var var | Var var, Structure s ->
      let context_from_closure closure =
        Region_tree.unsafe_max_by_level
          (ctx.region_node :: List.map closure ~f:ctx.region_of)
      in
      Suspended_var.fill var s ~context_from_closure;
      Structure s
    | Structure s1, Structure s2 ->
      Structure (F.merge ~ctx ~create:(fun s -> create (Structure s)) ~unify s1 s2)
  ;;

  let hmap t ~f =
    match t with
    | Var var -> Var var
    | Structure s -> Structure (F.hmap s ~f)
  ;;

  let iter t ~f =
    match t with
    | Var _ -> ()
    | Structure s -> F.iter s ~f
  ;;

  let fold t ~f ~init =
    match t with
    | Var _ -> init
    | Structure s -> F.fold s ~f ~init
  ;;
end

module S = struct
  include Types.S
end

module Type = struct
  include Types.U.Type

  type region_node = t Region_tree.node
  type region_tree = t Region_tree.t
  type region_path = t Region_tree.Path.t
  type region = t Region.t

  let id t = (structure t).id
  let inner t = (structure t).inner

  let set_inner t inner =
    let structure = structure t in
    set_structure t { structure with inner }
  ;;

  let status t = (structure t).status

  let set_status t status =
    let structure = structure t in
    set_structure t { structure with status }
  ;;

  let region t = Status.region (status t)

  let region_exn ?here type_ =
    Option.value_exn ?here ~message:"Type cannot be generic" (region type_)
  ;;

  let set_region t region_node =
    let status = status t in
    set_status t (Status.with_region status ~region_node)
  ;;

  let level t = Option.(region t >>| fun r -> r.level)

  let level_exn ?here type_ =
    Option.value_exn ?here ~message:"Type cannot be generic" (level type_)
  ;;

  let generalize t =
    let structure = structure t in
    set_structure t (S.generalize structure)
  ;;

  let guards t = (structure t).guards

  let resolve_guard t guard =
    let visited = Hash_set.create (module Identifier) in
    let guard_id = id guard in
    let rec loop t =
      let structure = structure t in
      let id = structure.id in
      if Hash_set.mem visited id
      then assert (not (Hash_set.mem structure.guards guard_id))
      else (
        Hash_set.add visited id;
        if Hash_set.mem structure.guards guard_id
        then (
          Hash_set.remove structure.guards guard_id;
          S.iter structure ~f:loop))
    in
    loop t
  ;;

  let add_guard' t guard = Hash_set.add (guards t) guard
  let add_guard t guard = add_guard' t (id guard)

  let add_guard_resolver t guard =
    match inner guard with
    | Var Empty | Structure _ -> assert false
    | Var (Empty_one_or_more_handlers handlers) ->
      set_inner
        guard
        (Var (Empty_one_or_more_handlers ((fun _ -> resolve_guard t guard) :: handlers)))
  ;;
end

module Scheme = struct
  type 'a t = { root : 'a } [@@deriving sexp_of]

  let body t = t.root
  let mono_scheme root = { root }
end

module Suspended_match = struct
  type 'a t =
    { matchee : 'a
    ; closure : 'a closure
    ; case : 'a Structure.Former.t -> unit
    }
  [@@deriving sexp_of]

  and 'a closure = { variables : 'a list } [@@deriving sexp_of]
end

module Global_state = struct
  type t = { id_source : Identifier.source }

  let create () = { id_source = Identifier.create_source () }
  let t_ref = ref (create ())
  let t () = !t_ref
  let id_source () = (t ()).id_source
  let reset () = t_ref := create ()
end

module Scheduler = struct
  type ty

  type job = unit -> unit
  and t = { job_queue : job Queue.t } [@@deriving sexp_of]

  let create () = { job_queue = Queue.create () }
  let t_ref = ref (create ())
  let t () = !t_ref
  let reset () = t_ref := create ()
  let schedule t job = Queue.enqueue t.job_queue job
  let schedule_all t jobs = Queue.enqueue_all t.job_queue jobs

  let run t =
    let rec loop () =
      match Queue.dequeue t.job_queue with
      | None -> ()
      | Some job ->
        job ();
        loop ()
    in
    loop ()
  ;;
end

module Suspended_var = struct
  type 'a t =
    | Empty
    | Empty_one_or_more_handlers of 'a handler list

  and 'a handler = 'a F.t -> unit [@@deriving sexp_of]

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

  let fill t s =
    match t with
    | Empty -> ()
    | Empty_one_or_more_handlers handlers ->
      List.iter handlers ~f:(fun handler ->
        Scheduler.schedule (Scheduler.t ()) (fun () -> handler s))
  ;;
end

module Suspended_first_order = struct
  type 'a t =
    | Var of 'a Suspended_var.t
    | Structure of 'a F.t
  [@@deriving sexp_of]

  type 'a ctx = 'a F.ctx

  exception Cannot_merge = F.Cannot_merge

  let merge ~ctx ~create ~unify t1 t2 =
    match t1, t2 with
    | Var var1, Var var2 -> Var (Suspended_var.merge var1 var2)
    | Structure s, Var var | Var var, Structure s ->
      Suspended_var.fill var s;
      Structure s
    | Structure s1, Structure s2 ->
      Structure (F.merge ~ctx ~create:(fun s -> create (Structure s)) ~unify s1 s2)
  ;;

  let hmap t ~f =
    match t with
    | Var var -> Var var
    | Structure s -> Structure (F.hmap s ~f)
  ;;

  let iter t ~f =
    match t with
    | Var _ -> ()
    | Structure s -> F.iter s ~f
  ;;

  let fold t ~f ~init =
    match t with
    | Var _ -> init
    | Structure s -> F.fold s ~f ~init
  ;;
end

module S = struct
  (* Def: A guard is a suspended variable, that if filled, may unify the current type in some
     suspended match.

     A generic variable has no guards
     An instance may have a guard
     A partially generic variable must have a guard
  *)

  type 'a t =
    { id : Identifier.t
    ; inner : 'a Suspended_first_order.t
    ; guards : Identifier.t Hash_set.t
    ; status : 'a Status.t
    }
  [@@deriving sexp_of]

  type 'a ctx =
    { id_source : Identifier.source
    ; region_node : 'a Region_tree.node
    ; partial_unify : 'a Suspended_first_order.t -> 'a -> unit
    }

  let create ~id_source ~region_node ?guards inner =
    { id = Identifier.create id_source
    ; inner
    ; guards =
        Option.value_or_thunk guards ~default:(fun () ->
          Hash_set.create (module Identifier))
    ; status = Instance region_node
    }
  ;;

  let generalize t =
    match t.status with
    | Generic -> assert false
    | Partial_generic _ when Hash_set.is_empty t.guards ->
      (* Generalizing a partial generic with no guards yields a generic *)
      { t with status = Generic }
    | Partial_generic _ -> t
    | Instance rn when not (Hash_set.is_empty t.guards) ->
      (* When a type is guarded, it is partially generalized *)
      { t with status = Partial_generic { region_node = rn; instances = [] } }
    | Instance _ -> { t with status = Generic }
  ;;

  let hmap t ~f = { t with inner = Suspended_first_order.hmap t.inner ~f }
  let fold t ~f ~init = Suspended_first_order.fold t.inner ~f ~init
  let iter t ~f = Suspended_first_order.iter t.inner ~f

  exception Cannot_merge = Suspended_first_order.Cannot_merge

  let merge ~ctx ~create:create_type ~unify t1 t2 =
    let create inner =
      let region_node = ctx.region_node in
      let type_ = create_type (create ~id_source:ctx.id_source ~region_node inner) in
      Region.register_type region_node.region type_;
      type_
    in
    let guards = Hash_set.union t1.guards t2.guards in
    let inner = Suspended_first_order.merge ~ctx:() ~create ~unify t1.inner t2.inner in
    let status =
      Status.merge t1.status t2.status ~partial_unify:(ctx.partial_unify inner)
    in
    { id = t1.id; inner; status; guards }
  ;;
end

module U = Tizoo_unifier.Unifier.Make (S)

module Type = struct
  include U.Type

  type region_node = t Region_tree.node
  type region_tree = t Region_tree.t
  type region_path = t Region_tree.Path.t
  type region = t Region.t
  type scheme = t Scheme.t

  let id t = (structure t).id
  let inner t = (structure t).inner

  let set_inner t inner =
    let structure = structure t in
    set_structure t { structure with inner }
  ;;

  let status t = (structure t).status

  let set_status t status =
    let structure = structure t in
    set_structure t { structure with status }
  ;;

  let region t = Status.region (status t)

  let region_exn ?here type_ =
    Option.value_exn ?here ~message:"Type cannot be generic" (region type_)
  ;;

  let set_region t region_node =
    let status = status t in
    set_status t (Status.with_region status ~region_node)
  ;;

  let level t = Option.(region t >>| fun r -> r.level)

  let level_exn ?here type_ =
    Option.value_exn ?here ~message:"Type cannot be generic" (level type_)
  ;;

  let generalize t =
    let structure = structure t in
    set_structure t (S.generalize structure)
  ;;

  let guards t = (structure t).guards

  let resolve_guard t guard =
    let visited = Hash_set.create (module Identifier) in
    let guard_id = id guard in
    let rec loop t =
      let structure = structure t in
      let id = structure.id in
      if Hash_set.mem visited id
      then assert (not (Hash_set.mem structure.guards guard_id))
      else (
        Hash_set.add visited id;
        if Hash_set.mem structure.guards guard_id
        then (
          Hash_set.remove structure.guards guard_id;
          S.iter structure ~f:loop))
    in
    loop t
  ;;

  let add_guard' t guard = Hash_set.add (guards t) guard
  let add_guard t guard = add_guard' t (id guard)

  let add_guard_resolver t guard =
    match inner guard with
    | Var Empty | Structure _ -> assert false
    | Var (Empty_one_or_more_handlers handlers) ->
      set_inner
        guard
        (Var (Empty_one_or_more_handlers ((fun _ -> resolve_guard t guard) :: handlers)))
  ;;
end

module State = struct
  type t = { region_node : Type.region_node }

  let region_node t = t.region_node [@@inline]
  let region t = (region_node t).region [@@inline]

  let create () =
    let region_tree = Region_tree.create (Global_state.id_source ()) in
    { region_node = Region_tree.root region_tree }
  ;;
end

open State

let enter ~state =
  { region_node =
      Region_tree.create_node
        ~id_source:(Global_state.id_source ())
        ~parent:(State.region_node state)
  }
;;

let create_type ~state ?guards inner =
  let type_ =
    Type.create
      (S.create
         ~id_source:(Global_state.id_source ())
         ~region_node:(State.region_node state)
         ?guards
         inner)
  in
  Region.register_type (State.region state) type_;
  type_
;;

let create_var ~state ?guards () = create_type ~state ?guards (Var Empty)
let create_former ~state ?guards former = create_type ~state ?guards (Structure former)

let suspend (suspended_match : Type.t Suspended_match.t) =
  match Type.inner suspended_match.matchee with
  | Var svar ->
    let svar = Suspended_var.add_handler svar suspended_match.case in
    Type.set_inner suspended_match.matchee (Var svar);
    List.iter suspended_match.closure.variables ~f:(fun type_ ->
      Type.add_guard type_ suspended_match.matchee;
      Type.add_guard_resolver type_ suspended_match.matchee)
  | Structure s -> suspended_match.case s
;;

let rec unify ~state type1 type2 =
  let unifier_ctx : _ S.ctx =
    { id_source = Global_state.id_source ()
    ; region_node = state.region_node
    ; partial_unify
    }
  in
  U.unify ~ctx:unifier_ctx type1 type2

and partial_unify (inner : _ Suspended_first_order.t) type_ =
  (* Copy structure (erasing svars) in inner *)
  let region_node = Type.region_exn ~here:[%here] type_ in
  let state = { region_node } in
  let inner_copy : Type.t Suspended_first_order.t =
    match inner with
    | Var _ -> Var Empty
    | Structure s ->
      Structure (Structure.Former.hmap s ~f:(fun _ -> create_var ~state ()))
  in
  let v = create_type ~state inner_copy in
  unify ~state v type_
;;

module Current_region = struct
  type t =
    { region : Type.region
    ; node : Type.region_node
    ; path : Type.region_path (** A path to the current region *)
    ; mem : Type.t -> bool
    (** Returns [true] if given type is a member of the current region *)
    }

  let of_state state =
    let region_node = State.region_node state in
    let path = Region_tree.Path.of_node region_node in
    let region = region_node.region in
    let mem =
      let set =
        Hash_set.of_list (module Identifier) (region.types |> List.map ~f:Type.id)
      in
      fun type_ -> Hash_set.mem set (Type.id type_)
    in
    { region; node = region_node; path; mem }
  ;;

  let max_region_node_by_level t r1 r2 =
    if Region_tree.Path.compare_node_by_level t.path r1 r2 < 0 then r2 else r1
  ;;
end

open Current_region

let update_types curr_region =
  Scheduler.(run (t ()));
  let visited = Hash_set.create (module Identifier) in
  let rec loop type_ guards r =
    assert (Region_tree.Path.mem curr_region.path r);
    let r' = Type.region_exn ~here:[%here] type_ in
    assert (Region_tree.Path.mem curr_region.path r');
    let id = Type.id type_ in
    if Hash_set.mem visited id
    then assert (Level.(r'.level <= r.level))
    else (
      Hash_set.add visited id;
      List.iter guards ~f:(Type.add_guard' type_);
      Type.set_region type_ (Current_region.max_region_node_by_level curr_region r r');
      if not (curr_region.mem type_)
      then
        (* [type_] is in parent regions *)
        assert (Level.(r'.level < curr_region.node.level))
      else (
        (* [type_] is in current region *)
        let guards = Type.guards type_ |> Hash_set.to_list in
        Type.structure type_ |> S.iter ~f:(fun type_ -> loop type_ guards r)))
  in
  List.iter curr_region.region.types ~f:(fun type_ ->
    loop
      type_
      (Type.guards type_ |> Hash_set.to_list)
      (Type.region_exn ~here:[%here] type_))
;;

exception Cannot_unsuspend_generic

let generalize curr_region =
  let curr_level = curr_region.node.level in
  let generics =
    curr_region.node.region.types
    |> List.filter ~f:(fun type_ ->
      Type.is_root type_
      &&
      let r = Type.region_exn ~here:[%here] type_ in
      if Level.(r.level < curr_level)
      then (
        (* Register [type_] in the region [r] *)
        Region.register_type r.region type_;
        (* All instances must unify to type *)
        (match Type.status type_ with
         | Partial_generic { region_node; instances } ->
           Type.set_status type_ (Instance region_node);
           List.iter instances ~f:(unify ~state:{ region_node } type_)
         | _ -> ());
        (* Filter the type from the result list *)
        false)
      else (
        assert (Level.(r.level = curr_level));
        (* Make the type generic *)
        Type.generalize type_;
        (* Cannot generalize unresolved svar *)
        (match Type.status type_, Type.inner type_ with
         | Generic, Var (Empty_one_or_more_handlers _) -> raise Cannot_unsuspend_generic
         | _ -> ());
        true))
  in
  let partial_generics, _generics =
    List.partition_tf generics ~f:(fun type_ ->
      match Type.status type_ with
      | Instance _ -> assert false
      | Partial_generic _ -> true
      | Generic -> false)
  in
  curr_region.region.types <- partial_generics
;;

let create_scheme root : Type.scheme = { root }

let update_and_generalize ~state () =
  let curr_region = Current_region.of_state state in
  update_types curr_region;
  generalize curr_region
;;

let exit ~state ?root () =
  update_and_generalize ~state ();
  let parent_region = Option.value_exn ~here:[%here] (State.region_node state).parent in
  { region_node = parent_region }, Option.map root ~f:create_scheme
;;

let instantiate ~state ({ generics; root } : Type.scheme) =
  let copies =
    generics
    |> List.filter_map ~f:(fun type_ ->
      let v =
        match Type.status type_ with
        | Generic -> Some (create_var ~state ())
        | Partial_generic { region_node; on_instance } ->
          let v =
            create_var
              ~state
              ~guards:(Hash_set.of_list (module Identifier) [ Type.id type_ ])
              ()
          in
          Type.set_status
            type_
            (Partial_generic
               { region_node
               ; on_instance = (fun () -> unify ~state type_ v) :: on_instance
               });
          Some v
        | Instance _ -> None
      in
      Option.map v ~f:(fun v -> Type.id type_, v))
    |> Hashtbl.of_alist_exn (module Identifier)
  in
  let copy type_ =
    let id = Type.id type_ in
    try Hashtbl.find_exn copies id with
    | Not_found_s _ -> type_
  in
  List.iter generics ~f:(fun type_ ->
    let structure = Type.structure type_ in
    let copied_type = Hashtbl.find_exn copies structure.id in
    Type.set_structure copied_type (S.hmap structure ~f:copy));
  copy root
;;
