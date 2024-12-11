open! Import
open Types

(* TODO: remove guards on partial generics if unified / generalized *)

let num_zombie_regions = ref 0
let reset_num_zombie_regions () = num_zombie_regions := 0

module Status = struct
  include Types.Status

  let set_region t rn =
    match t with
    | Instance _ -> Instance rn
    | Partial_generic { region_node = _; instances } ->
      Partial_generic { region_node = rn; instances }
    | Partial_instance { region_node = _; instances } ->
      Partial_instance { region_node = rn; instances }
    | Generic -> Generic
  ;;

  let region t =
    match t with
    | Instance rn -> Some rn
    | Partial_generic { region_node = rn; _ } -> Some rn
    | Partial_instance { region_node = rn; _ } -> Some rn
    | Generic -> None
  ;;

  let merge t1 t2 ~schedule_remove_instance_guard ~partial_unify_left ~partial_unify_right
    =
    match t1, t2 with
    | Generic, _ | _, Generic -> assert false
    | ( Partial_instance { region_node = rn1; instances = is1 }
      , Partial_instance { region_node = rn2; instances = is2 } ) ->
      let instances = is1 @ is2 in
      Partial_instance
        { region_node = Region_tree.nearest_common_ancestor rn1 rn2; instances }
    | ( Partial_instance { region_node = rn1; instances = is1 }
      , Partial_generic { region_node = rn2; instances = is2 } )
    | ( Partial_generic { region_node = rn1; instances = is1 }
      , Partial_instance { region_node = rn2; instances = is2 } ) ->
      Partial_instance
        { region_node = Region_tree.nearest_common_ancestor rn1 rn2
        ; instances = is1 @ is2
        }
    | Partial_instance { region_node = srn; instances }, Instance irn ->
      List.iter instances ~f:schedule_remove_instance_guard;
      List.iter instances ~f:partial_unify_right;
      Instance (Region_tree.nearest_common_ancestor srn irn)
    | Instance irn, Partial_instance { region_node = srn; instances } ->
      List.iter instances ~f:schedule_remove_instance_guard;
      List.iter instances ~f:partial_unify_left;
      Instance (Region_tree.nearest_common_ancestor srn irn)
    | Partial_generic { region_node = srn; instances }, Instance irn ->
      List.iter instances ~f:schedule_remove_instance_guard;
      List.iter instances ~f:partial_unify_right;
      Instance (Region_tree.nearest_common_ancestor srn irn)
    | Instance irn, Partial_generic { region_node = srn; instances } ->
      List.iter instances ~f:schedule_remove_instance_guard;
      List.iter instances ~f:partial_unify_left;
      Instance (Region_tree.nearest_common_ancestor srn irn)
    | ( Partial_generic { region_node = rn1; instances = is1 }
      , Partial_generic { region_node = rn2; instances = is2 } ) ->
      let instances = is1 @ is2 in
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

  let fill t s ~schedule =
    match t with
    | Empty -> ()
    | Empty_one_or_more_handlers handlers ->
      print_endline "svar filled and handlers fired";
      List.iter handlers ~f:(fun handler -> schedule (fun () -> handler.run s))
  ;;
end

module Suspended_first_order = struct
  include Types.Suspended_first_order

  type 'a ctx =
    { schedule : (unit -> unit) -> unit
    ; super : 'a F.ctx
    }

  exception Cannot_merge = F.Cannot_merge

  let merge ~ctx ~create ~unify v1 v2 t1 t2 =
    match t1, t2 with
    | Var var1, Var var2 -> Var (Suspended_var.merge var1 var2)
    | Structure s, Var var | Var var, Structure s ->
      Suspended_var.fill var s ~schedule:ctx.schedule;
      Structure s
    | Structure s1, Structure s2 ->
      Structure (F.merge ~ctx ~create:(fun s -> create (Structure s)) ~unify v1 v2 s1 s2)
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

module Region = struct
  include Types.Region

  let create () = { types = []; status = Alive }
  let register_type t type_ = t.types <- type_ :: t.types

  let default_status t rn : Status.t =
    match t.status with
    | Alive -> Instance rn
    | Zombie -> Partial_instance { region_node = rn; instances = [] }
    | Dead -> assert false
  ;;
end

module S = struct
  include Types.S

  let create
    ~id_source
    ~(region_node : _ Region_tree.node)
    ?(guards = Hash_set.create (module Identifier))
    inner
    =
    let status = Region.default_status region_node.region region_node in
    { id = Identifier.create id_source; status; guards; inner }
  ;;

  let generalize t =
    match t.status with
    | Generic -> assert false
    | Partial_generic _ -> t
    | Partial_instance _ when Hash_set.is_empty t.guards -> { t with status = Generic }
    | Partial_instance { region_node; instances } ->
      { t with status = Partial_generic { region_node; instances } }
    | Instance _ when Hash_set.is_empty t.guards ->
      print_endline "Type made generic";
      { t with status = Generic }
    | Instance region_node ->
      print_endline "Type is made partial generic";
      Hash_set.iter t.guards ~f:(fun id -> print_s [%message "id" (id : Identifier.t)]);
      { t with status = Partial_generic { region_node; instances = [] } }
  ;;

  let partial_generalize t ~f =
    match t.status with
    | Partial_generic { instances; _ } when Hash_set.is_empty t.guards ->
      List.iter instances ~f;
      { t with status = Generic }
    | _ -> t
  ;;

  type 'a ctx =
    { id_source : Identifier.source
    ; region_node : Type.t Region_tree.node
    ; remove_instance_guard : Type.t -> Identifier.t -> unit
    ; super : 'a Suspended_first_order.ctx
    ; magic : Type.t -> 'a
    ; magic2 : 'a -> Type.t
    }

  exception Cannot_merge = Suspended_first_order.Cannot_merge

  let hmap t ~f = { t with inner = Suspended_first_order.hmap t.inner ~f }
  let iter t ~f = Suspended_first_order.iter t.inner ~f
  let fold t ~init ~f = Suspended_first_order.fold t.inner ~init ~f

  let merge ~ctx ~create:create_type ~unify v1 v2 t1 t2 =
    let create inner =
      let region_node = ctx.region_node in
      let type_ = create_type (create ~id_source:ctx.id_source ~region_node inner) in
      Region.register_type region_node.region (ctx.magic2 type_);
      type_
    in
    let partial_unify_left (_, v) = unify v1 (ctx.magic v) in
    let partial_unify_right (_, v) = unify (ctx.magic v) v2 in
    let schedule = ctx.super.schedule in
    let schedule_remove_instance_guard (id, v) =
      schedule (fun () -> ctx.remove_instance_guard v id)
    in
    let status =
      Status.merge
        t1.status
        t2.status
        ~schedule_remove_instance_guard
        ~partial_unify_left
        ~partial_unify_right
    in
    let inner =
      Suspended_first_order.merge ~ctx:ctx.super ~create ~unify v1 v1 t1.inner t2.inner
    in
    let guards = Hash_set.union t1.guards t2.guards in
    { id = t1.id; status; inner; guards }
  ;;
end

module Scheme = struct
  type 'a t = { root : 'a } [@@deriving sexp_of]

  let body t = t.root
  let mono_scheme root = { root }
end

module Type = struct
  include Types.U.Type

  type region_node = t Region_tree.node [@@deriving sexp_of]
  type region_tree = t Region_tree.t [@@deriving sexp_of]
  type region_path = t Region_tree.Path.t [@@deriving sexp_of]
  type region = t Region.t [@@deriving sexp_of]
  type scheme = t Scheme.t [@@deriving sexp_of]

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
    set_status t (Status.set_region status region_node)
  ;;

  let level t = Option.(region t >>| fun r -> r.level)

  let level_exn ?here type_ =
    Option.value_exn ?here ~message:"Type cannot be generic" (level type_)
  ;;

  let generalize t =
    let structure = structure t in
    set_structure t (S.generalize structure)
  ;;

  let partial_generalize t ~f =
    let structure = structure t in
    set_structure t (S.partial_generalize structure ~f)
  ;;

  let guards t = (structure t).guards

  let resolve_guard t guard ~generalize =
    print_s [%message "Removing guard" (t : t) (guard : Identifier.t)];
    let visited = Hash_set.create (module Identifier) in
    let visited_regions = Hashtbl.create (module Identifier) in
    let rec loop t =
      let structure = structure t in
      let id = structure.id in
      if Hash_set.mem visited id
      then assert (not (Hash_set.mem structure.guards guard))
      else (
        Hash_set.add visited id;
        if Hash_set.mem structure.guards guard
        then (
          let region = region_exn ~here:[%here] t in
          Hashtbl.set visited_regions ~key:region.id ~data:region;
          Hash_set.remove structure.guards guard;
          S.iter structure ~f:loop))
    in
    loop t;
    Hashtbl.iter visited_regions ~f:generalize
  ;;

  let add_guard t guard = Hash_set.add (guards t) guard

  let add_guard_resolver t guard svar ~generalize =
    match inner svar with
    | Var Empty | Structure _ -> assert false
    | Var (Empty_one_or_more_handlers handlers) ->
      (* Implicit invariant: the guards are removed after the handler is run *)
      set_inner
        svar
        (Var
           (Empty_one_or_more_handlers
              ({ run = (fun _ -> resolve_guard t guard ~generalize) } :: handlers)))
  ;;
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

module Scheduler = struct
  type job = unit -> unit
  and t = { job_queue : job Queue.t } [@@deriving sexp_of]

  let create () = { job_queue = Queue.create () }
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

module Unify = U.Make_unify (S)

module State = struct
  type t =
    { id_source : Identifier.source
    ; scheduler : Scheduler.t
    ; region_node : Type.region_node
    }

  let region_node t = t.region_node [@@inline]
  let region t = (region_node t).region [@@inline]

  let create () =
    let id_source = Identifier.create_source () in
    let scheduler = Scheduler.create () in
    let region_tree = Region_tree.create ~id_source ~region:(Region.create ()) in
    { region_node = Region_tree.root region_tree; id_source; scheduler }
  ;;
end

module Current_region = struct
  type t =
    { region : Type.region
    ; node : Type.region_node
    ; path : Type.region_path (** A path to the current region *)
    ; mem : Type.t -> bool
    (** Returns [true] if given type is a member of the current region *)
    }
  [@@deriving sexp_of]

  let of_region_node region_node =
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

  let of_state state = of_region_node (State.region_node state)

  let max_region_node_by_level t r1 r2 =
    if Region_tree.Path.compare_node_by_level t.path r1 r2 < 0 then r2 else r1
  ;;
end

open Current_region
open State

let enter ~state =
  { state with
    region_node =
      Region_tree.create_node
        ~id_source:state.id_source
        ~parent:(State.region_node state)
        ~region:(Region.create ())
  }
;;

let create_type ~state ?guards inner =
  let type_ =
    Type.create
      (S.create
         ~id_source:state.id_source
         ~region_node:(State.region_node state)
         ?guards
         inner)
  in
  Region.register_type (State.region state) type_;
  type_
;;

let create_var ~state ?guards () = create_type ~state ?guards (Var Empty)
let create_former ~state ?guards former = create_type ~state ?guards (Structure former)

exception Cannot_unsuspend_generic

let partial_copy ~state type_ =
  let copies = Hashtbl.create (module Identifier) in
  let rec loop type_ =
    let structure = Type.structure type_ in
    match structure.status with
    | Instance _ | Partial_instance _ -> type_
    | Generic | Partial_generic _ ->
      let id = structure.id in
      (try Hashtbl.find_exn copies id with
       | Not_found_s _ ->
         let copy = create_var ~state () in
         Hashtbl.set copies ~key:id ~data:copy;
         (match structure.status with
          | Generic ->
            Type.set_inner copy (Suspended_first_order.hmap structure.inner ~f:loop)
          | _ -> ());
         copy)
  in
  loop type_
;;

let rec unify ~state type1 type2 =
  let unifier_ctx : _ S.ctx =
    { id_source = state.id_source
    ; region_node = state.region_node
    ; magic = Fn.id
    ; magic2 = Fn.id
    ; super = { schedule = Scheduler.schedule state.scheduler; super = () }
    ; remove_instance_guard =
        Type.resolve_guard ~generalize:(update_and_generalize_region_node ~state)
    }
  in
  Unify.unify ~ctx:unifier_ctx type1 type2;
  Scheduler.run state.scheduler

and suspend ~state (suspended_match : Type.t Suspended_match.t) =
  match Type.inner suspended_match.matchee with
  | Var svar ->
    let guard = Identifier.create state.id_source in
    let svar = Suspended_var.add_handler svar { run = suspended_match.case } in
    Type.set_inner suspended_match.matchee (Var svar);
    print_s
      [%message "Suspend on" (suspended_match.matchee : Type.t) (guard : Identifier.t)];
    List.iter suspended_match.closure.variables ~f:(fun type_ ->
      Type.add_guard type_ guard;
      Type.add_guard_resolver
        type_
        guard
        suspended_match.matchee
        ~generalize:(update_and_generalize_region_node ~state))
  | Structure s -> suspended_match.case s

and update_types curr_region =
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
      List.iter guards ~f:(Type.add_guard type_);
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

and generalize ~state curr_region =
  print_s [%message "Generalizing region" (curr_region : Current_region.t)];
  let orig_status = curr_region.region.status in
  let curr_level = curr_region.node.level in
  let generics =
    curr_region.node.region.types
    |> List.filter ~f:(fun type_ ->
      Type.is_representative type_
      &&
      let r = Type.region_exn ~here:[%here] type_ in
      if Level.(r.level < curr_level)
      then (
        (* Register [type_] in the region [r] *)
        Region.register_type r.region type_;
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
  let generalize_later = Hashtbl.create (module Identifier) in
  List.iter generics ~f:(fun generic ->
    Type.partial_generalize generic ~f:(fun (guard, instance) ->
      let region_node = Type.region_exn ~here:[%here] instance in
      let state = { state with region_node } in
      let copy = partial_copy ~state generic in
      unify ~state copy instance;
      Type.resolve_guard instance guard ~generalize:(fun region_node ->
        Hashtbl.set generalize_later ~key:region_node.id ~data:region_node);
      Hashtbl.set generalize_later ~key:region_node.id ~data:region_node));
  let partial_generics, _generics =
    List.partition_tf generics ~f:(fun type_ ->
      match Type.status type_ with
      | Instance _ -> assert false
      | Partial_instance _ -> assert false
      | Partial_generic _ -> true
      | Generic -> false)
  in
  curr_region.region.types <- partial_generics;
  if List.is_empty partial_generics
  then (
    (match orig_status with
     | Zombie ->
       print_endline "Generalizing zombie region into dead region";
       decr num_zombie_regions
     | _ -> ());
    curr_region.region.status <- Dead)
  else (
    (match orig_status with
     | Alive ->
       print_endline "Generaizing alive region into zombie region";
       incr num_zombie_regions
     | _ -> ());
    print_s [%message "Zombie region" (partial_generics : Type.t list)];
    curr_region.region.status <- Zombie);
  (* Do some left over generalization work *)
  List.iter (Hashtbl.data generalize_later) ~f:(update_and_generalize_region_node ~state)

and update_and_generalize ~state curr_region =
  update_types curr_region;
  generalize ~state curr_region

and update_and_generalize_region_node ~state region_node =
  match region_node.region.status with
  | Zombie ->
    let curr_region = Current_region.of_region_node region_node in
    update_and_generalize ~state curr_region
  | _ -> ()
;;

let create_scheme root : Type.t Scheme.t = { root }

let update_and_generalize ~state () =
  let curr_region = Current_region.of_state state in
  update_and_generalize ~state curr_region
;;

let exit ~state ?root () =
  update_and_generalize ~state ();
  let parent_region = Option.value_exn ~here:[%here] (State.region_node state).parent in
  { state with region_node = parent_region }, Option.map root ~f:create_scheme
;;

let instantiate ~state ({ root } : Type.t Scheme.t) =
  let copies = Hashtbl.create (module Identifier) in
  let rec loop type_ =
    let structure = Type.structure type_ in
    match structure.status with
    | Instance _ | Partial_instance _ -> type_
    | _ ->
      (try Hashtbl.find_exn copies structure.id with
       | Not_found_s _ ->
         (* Register instance if partial *)
         let copy =
           match structure.status with
           | Partial_generic { instances; region_node } ->
             let guard = structure.id in
             let v =
               create_var
                 ~state
                 ~guards:(Hash_set.of_list (module Identifier) [ guard ])
                 ()
             in
             Type.set_structure
               type_
               { structure with
                 status =
                   Partial_generic { region_node; instances = (guard, v) :: instances }
               };
             v
           | _ -> create_var ~state ()
         in
         Hashtbl.set copies ~key:structure.id ~data:copy;
         Type.set_inner copy (Suspended_first_order.hmap structure.inner ~f:loop);
         copy)
  in
  loop root
;;
