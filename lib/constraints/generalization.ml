open! Import
module F = Structure.Former

module Region = struct
  (** The [Generalization] module manages the generalisation of graphical types.

      Each type belongs to a 'region', which indicates where those types are
      existentially bound in the solver's stack. *)
  type 'a t =
    { mutable status : status
    ; mutable types : 'a list
    }
  [@@deriving sexp_of]

  and status =
    | Alive (** A region is 'alive' if is yet to be generalized *)
    | Dead (** A region is 'dead' if it is 'fully' generalized *)
    | Zombie (** A region is a 'zombie' if is 'partially' generalized *)
  [@@deriving sexp_of]

  let create () = { types = []; status = Alive }
  let register_type t type_ = t.types <- type_ :: t.types

  module Tree = struct
    type 'a node = 'a t Tree.node [@@deriving sexp_of]
    type 'a path = 'a t Tree.Path.t [@@deriving sexp_of]
    type nonrec 'a t = 'a t Tree.t [@@deriving sexp_of]

    (** A type used to trick ppx_sexp_conv *)
    type 'a sexp_identifier_node = 'a node

    let sexp_of_sexp_identifier_node _sexp_of_a (node : 'a sexp_identifier_node) =
      [%sexp_of: Identifier.t] node.id
    ;;

    let region (t : 'a node) = t.value
  end
end

(* TODO: Refactor guards into something better *)
module Guard = Identifier

module Partial_status = struct
  type 'a t =
    { region_node : 'a Region.Tree.sexp_identifier_node
    ; instances : (Guard.t * 'a) list
    ; kind : kind
    }
  [@@deriving sexp_of]

  and kind =
    | Instance
    | Generic
  [@@deriving sexp_of]

  let merge_kind k1 k2 =
    match k1, k2 with
    | Instance, _ | _, Instance -> Instance
    | Generic, Generic -> Generic
  ;;

  let merge t1 t2 =
    { region_node = Tree.nearest_common_ancestor t1.region_node t2.region_node
    ; instances = t1.instances @ t2.instances
    ; kind = merge_kind t1.kind t2.kind
    }
  ;;
end

module Status = struct
  type 'a t =
    | Instance of 'a Region.Tree.sexp_identifier_node
    | Partial of 'a Partial_status.t
    | Generic
  [@@deriving sexp_of]

  let set_region t rn =
    match t with
    | Instance _ -> Instance rn
    | Partial p -> Partial { p with region_node = rn }
    | Generic -> Generic
  ;;

  let region t =
    match t with
    | Instance rn -> Some rn
    | Partial { region_node = rn; _ } -> Some rn
    | Generic -> None
  ;;

  let merge t1 t2 ~partial_unify =
    match t1, t2 with
    | Generic, _ | _, Generic -> assert false
    | Partial p1, Partial p2 -> Partial (Partial_status.merge p1 p2)
    | Partial { region_node = rn1; instances; kind = _ }, Instance rn2 ->
      List.iter instances ~f:partial_unify;
      Instance (Tree.nearest_common_ancestor rn1 rn2)
    | Instance rn1, Partial { region_node = rn2; instances; kind = _ } ->
      List.iter instances ~f:partial_unify;
      Instance (Tree.nearest_common_ancestor rn1 rn2)
    | Instance rn1, Instance rn2 -> Instance (Tree.nearest_common_ancestor rn1 rn2)
  ;;

  let of_region_node region_node =
    match (Region.Tree.region region_node).status with
    | Alive -> Instance region_node
    | Zombie -> Partial { region_node; instances = []; kind = Instance }
    | Dead -> assert false
  ;;
end

module S = struct
  module Inner = Structure.Suspended_first_order (F)

  type 'a t =
    { id : Identifier.t
    ; guards : Identifier.t Hash_set.t
    ; inner : 'a Inner.t
    ; status : 'a Status.t
    }
  [@@deriving sexp_of]

  let create ~id_source ~region_node ?(guards = Hash_set.create (module Identifier)) inner
    =
    { id = Identifier.create id_source
    ; status = Status.of_region_node region_node
    ; guards
    ; inner
    }
  ;;

  let generalize t =
    let if_guarded ~then_ ~else_ =
      let status = if not (Hash_set.is_empty t.guards) then then_ else else_ in
      { t with status }
    in
    match t.status with
    | Generic -> assert false
    | Partial { kind = Generic; _ } ->
      (* [generalize] cannot generalize partial generics. See [partial_generalize] *)
      t
    | Partial ({ kind = Instance; instances; _ } as p) ->
      let status : _ Status.t =
        (* [generalize] cannot generalize partial generics *or* partial instances that have instances *)
        if not (Hash_set.is_empty t.guards && List.is_empty instances)
        then Partial { p with kind = Generic }
        else Generic
      in
      { t with status }
    | Instance region_node ->
      if_guarded
        ~then_:(Partial { region_node; instances = []; kind = Generic })
        ~else_:Generic
  ;;

  let partial_generalize t ~f =
    match t.status with
    | Partial { kind = Generic; instances; _ } when Hash_set.is_empty t.guards ->
      (* Generalize a partial generic to a generic when it is unguarded. *)
      List.iter instances ~f;
      { t with status = Generic }
    | _ -> t
  ;;

  type 'a ctx =
    { id_source : Identifier.source
    ; curr_region : 'a Region.Tree.node
    ; schedule_remove_guard : 'a -> Guard.t -> unit
    ; super : 'a Inner.ctx
    }

  exception Cannot_merge = Inner.Cannot_merge

  let map t ~f = { t with inner = Inner.map t.inner ~f }
  let iter t ~f = Inner.iter t.inner ~f
  let fold t ~init ~f = Inner.fold t.inner ~init ~f

  let merge ~ctx ~create:create_type ~unify ~type1 ~type2 t1 t2 =
    let create inner =
      let region_node = ctx.curr_region in
      let type_ = create_type (create ~id_source:ctx.id_source ~region_node inner) in
      Region.(register_type (Tree.region region_node) type_);
      type_
    in
    let partial_unify (guard, inst) =
      (* It doesn't matter which type ([type1] or [type2]) we pick, since after
         the current unification is successful, [type1] and [type2] will refer to
         the *same* type. *)
      unify type2 inst;
      ctx.schedule_remove_guard inst guard
    in
    let status = Status.merge t1.status t2.status ~partial_unify in
    let inner =
      Inner.merge ~ctx:ctx.super ~create ~unify ~type1 ~type2 t1.inner t2.inner
    in
    let guards = Hash_set.union t1.guards t2.guards in
    { id = t1.id; status; inner; guards }
  ;;
end

module Scheme = struct
  type 'a t =
    { root : 'a
    ; region_node : 'a Region.Tree.sexp_identifier_node option
    }
  [@@deriving sexp_of]

  let body t = t.root
  let mono_scheme root = { root; region_node = None }
end

module Type = struct
  include Unifier.Make (S)
  include Type

  type region_node = t Region.Tree.node [@@deriving sexp_of]

  type sexp_identifier_region_node = t Region.Tree.sexp_identifier_node
  [@@deriving sexp_of]

  type region_tree = t Region.Tree.t [@@deriving sexp_of]
  type region_path = t Region.Tree.path [@@deriving sexp_of]
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
  let add_guard t guard = Hash_set.add (guards t) guard

  let add_handler t handler =
    match inner t with
    | Var svar ->
      let svar = S.Inner.Var.add_handler svar handler in
      set_inner t (Var svar)
    | Structure _ -> assert false
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

module Generalization_tree : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val visit_region : t -> Type.region_node -> unit

  (** Note: [f rn] may update [t], but only the ancestors of [rn] stored in [t] *)
  val generalize_region : t -> Type.region_node -> f:(Type.region_node -> unit) -> unit
end = struct
  type t =
    { entered_map : (Identifier.t, (Identifier.t, Type.region_node) Hashtbl.t) Hashtbl.t
    (** Maps node identifiers to immediate entered descendants *)
    }
  [@@deriving sexp_of]

  let create () = { entered_map = Hashtbl.create (module Identifier) }

  let rec find_closest_entered_ancestor t (node : Type.region_node) =
    match node.parent with
    | None -> None
    | Some parent ->
      if Hashtbl.mem t.entered_map parent.id
      then Some parent
      else find_closest_entered_ancestor t parent
  ;;

  let visit_region t (rn : Type.region_node) =
    if not (Hashtbl.mem t.entered_map rn.id)
    then (
      (* Enter [rn] *)
      let imm_descendants = Hashtbl.create (module Identifier) in
      Hashtbl.set t.entered_map ~key:rn.id ~data:imm_descendants;
      match find_closest_entered_ancestor t rn with
      | None -> ()
      | Some anc ->
        (* TODO: optimisation, if we know [rn] is a new region, then we can ignore this *)
        (* Reparent decendents of [rn] *)
        let anc_descendants = Hashtbl.find_exn t.entered_map anc.id in
        Hashtbl.filter_inplace anc_descendants ~f:(fun imm_descendant ->
          let imm_anc =
            find_closest_entered_ancestor t imm_descendant
            |> Option.value_exn ~here:[%here]
          in
          if Identifier.(imm_anc.id = rn.id)
          then (
            Hashtbl.set imm_descendants ~key:imm_descendant.id ~data:imm_descendant;
            false)
          else true);
        (* Register [rn] as a descendant of [anc] *)
        Hashtbl.set anc_descendants ~key:rn.id ~data:rn)
  ;;

  let generalize_region t rn ~f =
    let rec visit : Type.region_node -> unit =
      fun rn ->
      match Hashtbl.find t.entered_map rn.id with
      | None -> ()
      | Some imm_descendants ->
        let rec loop () =
          match Hashtbl.choose imm_descendants with
          | None -> ()
          | Some (rn_id, rn) ->
            (* Problem: What if visiting [rn] triggers [visit_region rn]? *)
            visit rn;
            Hashtbl.remove imm_descendants rn_id;
            loop ()
        in
        loop ();
        (* Remove entry :) *)
        Hashtbl.remove t.entered_map rn.id;
        (match find_closest_entered_ancestor t rn with
         | None -> ()
         | Some anc -> Hashtbl.remove (Hashtbl.find_exn t.entered_map anc.id) rn.id);
        (* Generalize *)
        f rn
    in
    visit rn
  ;;
end

module State = struct
  type t =
    { id_source : (Identifier.source[@sexp.opaque])
    ; scheduler : Scheduler.t
    ; generalization_tree : Generalization_tree.t
    ; mutable num_zombie_regions : int
    }
  [@@deriving sexp_of]

  let create () =
    let id_source = Identifier.create_source () in
    let scheduler = Scheduler.create () in
    { id_source
    ; scheduler
    ; generalization_tree = Generalization_tree.create ()
    ; num_zombie_regions = 0
    }
  ;;

  let incr_zombie_regions t = t.num_zombie_regions <- t.num_zombie_regions + 1
  let decr_zombie_regions t = t.num_zombie_regions <- t.num_zombie_regions - 1
end

module Unify = Type.Make_unify (S)

module Young_region = struct
  type t =
    { region : Type.region
    ; node : Type.region_node
    ; path : Type.region_path (** A path to the current region *)
    ; mem : Type.t -> bool
    (** Returns [true] if given type is a member of the current region *)
    }
  [@@deriving sexp_of]

  let of_region_node region_node =
    let path = Tree.Path.of_node region_node in
    let region = Region.Tree.region region_node in
    let mem =
      let set =
        Hash_set.of_list (module Identifier) (region.types |> List.map ~f:Type.id)
      in
      fun type_ -> Hash_set.mem set (Type.id type_)
    in
    { region; node = region_node; path; mem }
  ;;

  let min_region_node_by_level t r1 r2 =
    if Tree.Path.compare_node_by_level t.path r1 r2 < 0 then r1 else r2
  ;;
end

open State

let visit_region ~state rn = Generalization_tree.visit_region state.generalization_tree rn

let root_region ~state =
  let rn = Tree.create ~id_source:state.id_source (Region.create ()) |> Tree.root in
  visit_region ~state rn;
  rn
;;

let enter_region ~state curr_region =
  let rn =
    Tree.create_node ~id_source:state.id_source ~parent:curr_region (Region.create ())
  in
  visit_region ~state rn;
  rn
;;

let create_type ~state ~curr_region ?guards inner =
  let type_ =
    Type.create
      (S.create ~id_source:state.id_source ~region_node:curr_region ?guards inner)
  in
  Region.(register_type (Tree.region curr_region) type_);
  type_
;;

let create_var ~state ~curr_region ?guards () =
  create_type ~state ~curr_region ?guards (Var Empty)
;;

let create_former ~state ~curr_region ?guards former =
  create_type ~state ~curr_region ?guards (Structure former)
;;

exception Cannot_unsuspend_generic

let partial_copy ~state ~curr_region type_ =
  (* Copy generics fully, partial generics are shallowly copied (only fresh vars) *)
  let copies = Hashtbl.create (module Identifier) in
  let rec loop type_ =
    let structure = Type.structure type_ in
    match structure.status with
    | Instance _ | Partial { kind = Instance; _ } -> type_
    | Generic | Partial { kind = Generic; _ } ->
      let id = structure.id in
      (try Hashtbl.find_exn copies id with
       | Not_found_s _ ->
         let copy = create_var ~state ~curr_region () in
         Hashtbl.set copies ~key:id ~data:copy;
         (match structure.status with
          | Generic -> Type.set_inner copy (S.Inner.map structure.inner ~f:loop)
          | _ -> ());
         copy)
  in
  loop type_
;;

let rec unify ~state ~curr_region type1 type2 =
  let schedule_handler s (handler : _ S.Inner.Var.handler) =
    Scheduler.schedule state.scheduler (fun () -> handler.run s)
  in
  let schedule_remove_guard inst guard =
    Scheduler.schedule state.scheduler (fun () -> remove_guard ~state inst guard)
  in
  let unifier_ctx : _ S.ctx =
    { id_source = state.id_source
    ; curr_region
    ; super = { schedule_handler; super = () }
    ; schedule_remove_guard
    }
  in
  Unify.unify ~ctx:unifier_ctx type1 type2

and remove_guard ~state t guard =
  let visited = Hash_set.create (module Identifier) in
  let rec loop t =
    [%log.global.debug "Removing guard" (guard : Identifier.t) (t : Type.t)];
    let structure = Type.structure t in
    let id = structure.id in
    if Hash_set.mem visited id
    then assert (not (Hash_set.mem structure.guards guard))
    else (
      Hash_set.add visited id;
      if Hash_set.mem structure.guards guard
      then (
        [%log.global.debug "Visiting children" (t : Type.t)];
        let region = Type.region_exn ~here:[%here] t in
        visit_region ~state region;
        Hash_set.remove structure.guards guard;
        S.iter structure ~f:loop))
  in
  loop t

and suspend ~state:_ ~guard ({ matchee; case; closure } : Type.t Suspended_match.t) =
  match Type.inner matchee with
  | Var _ ->
    Type.add_handler matchee { run = case };
    (* Add guards and remove handlers for each variable in closure *)
    List.iter closure.variables ~f:(fun type_ -> Type.add_guard type_ guard)
  | Structure s -> case s

and update_types ~state (young_region : Young_region.t) =
  [%log.global.debug "Updating types" (young_region : Young_region.t)];
  let visited = Hash_set.create (module Identifier) in
  let rec loop type_ guards r =
    [%log.global.debug
      "Visiting"
        (type_ : Type.t)
        (guards : Guard.t list)
        (r : Type.sexp_identifier_region_node)];
    assert (Tree.Path.mem young_region.path r);
    let r' = Type.region_exn ~here:[%here] type_ in
    [%log.global.debug "Region of type_" (r' : Type.sexp_identifier_region_node)];
    assert (Tree.Path.mem young_region.path r');
    let id = Type.id type_ in
    if Hash_set.mem visited id
    then (
      [%log.global.debug "Already visited" (id : Identifier.t)];
      assert (Tree.Level.(r'.level <= r.level)))
    else (
      [%log.global.debug "Not previously visited" (id : Identifier.t)];
      Hash_set.add visited id;
      List.iter guards ~f:(Type.add_guard type_);
      [%log.global.debug "Marked as visited and added guards"];
      (* Visiting and updating region *)
      if Tree.Path.compare_node_by_level young_region.path r r' < 0
      then (
        visit_region ~state r;
        Type.set_region type_ r;
        [%log.global.debug "Setting region to" (r : Type.sexp_identifier_region_node)]);
      (* Handle children *)
      if not (young_region.mem type_)
      then (
        [%log.global.debug "Type not in young region" (id : Identifier.t)];
        (* [type_] is in parent regions *)
        assert (Tree.Level.(r'.level < young_region.node.level)))
      else (
        [%log.global.debug "Type in young region, visiting children" (id : Identifier.t)];
        (* [type_] is in current region *)
        let guards = Type.guards type_ |> Hash_set.to_list in
        Type.structure type_ |> S.iter ~f:(fun type_ -> loop type_ guards r)))
  in
  List.iter young_region.region.types ~f:(fun type_ ->
    loop
      type_
      (Type.guards type_ |> Hash_set.to_list)
      (Type.region_exn ~here:[%here] type_))

and generalize_young_region ~state (young_region : Young_region.t) =
  [%log.global.debug "Generalizing young region" (young_region : Young_region.t)];
  let orig_status = young_region.region.status in
  assert (
    match orig_status with
    | Dead -> false
    | _ -> true);
  let young_level = young_region.node.level in
  (* Generalize the region *)
  let generics =
    young_region.region.types
    |> List.filter ~f:(fun type_ ->
      Type.is_representative type_
      &&
      ([%log.global.debug "Visiting type" (type_ : Type.t)];
       let r = Type.region_exn ~here:[%here] type_ in
       [%log.global.debug "Region of type_" (r : Type.sexp_identifier_region_node)];
       if Tree.Level.(r.level < young_level)
       then (
         [%log.global.debug "Type is not generic"];
         (* Register [type_] in the region [r] *)
         visit_region ~state r;
         Region.(register_type (Tree.region r) type_);
         (* Filter the type from the result list *)
         false)
       else (
         [%log.global.debug "Type is generic"];
         assert (Tree.Level.(r.level = young_level));
         (* Make the type generic *)
         Type.generalize type_;
         (* Cannot generalize unresolved svar *)
         (match Type.status type_, Type.inner type_ with
          | Generic, Var (Empty_one_or_more_handlers _) -> raise Cannot_unsuspend_generic
          | _ -> ());
         true)))
  in
  [%log.global.debug "Generics for young region" (generics : Type.t list)];
  (* Deal with partial generics that can be generalized to full generics *)
  [%log.global.debug
    "Partially generalizing remaining partial generics" (generics : Type.t list)];
  List.iter generics ~f:(fun generic ->
    [%log.global.debug "Partially generalizing generic" (generic : Type.t)];
    Type.partial_generalize generic ~f:(fun (guard, instance) ->
      [%log.global.debug
        "Generalizing the partial generic, visiting instance"
          (generic : Type.t)
          (guard : Guard.t)
          (instance : Type.t)];
      (* The partial generic that links to [instance] has been fully generalized :) *)
      let curr_region = Type.region_exn ~here:[%here] instance in
      (* [curr_region] is a sibling or parent of the current region. It cannot be a decsendant *)
      visit_region ~state curr_region;
      (* Perform a partial copy on the generic to ensure the instance has the generalized
         structure and then unify *)
      let copy = partial_copy ~state ~curr_region generic in
      (* NOTE: Scheduler jobs that are queued by [unify] and [remove_guard] only visit siblings or parents. *)
      unify ~state ~curr_region copy instance;
      (* Remove the guard *)
      remove_guard ~state instance guard);
    [%log.global.debug "Partially generalized generic" (generic : Type.t)]);
  [%log.global.debug "Changes" (generics : Type.t list)];
  (* Update the region to only contain the remaining partial generics *)
  let partial_generics, generics =
    List.partition_tf generics ~f:(fun type_ ->
      match Type.status type_ with
      | Instance _ | Partial { kind = Instance; _ } ->
        (* No instances are left in the region *)
        assert false
      | Partial { kind = Generic; _ } -> true
      | Generic -> false)
  in
  ignore (generics : Type.t list);
  young_region.region.types <- partial_generics;
  [%log.global.debug "Updated region" (young_region.region : Type.region)];
  if List.is_empty partial_generics
  then (
    (match orig_status with
     | Zombie ->
       [%log.global.debug "Was a zombie region, now is dead"];
       State.decr_zombie_regions state
     | _ -> ());
    young_region.region.status <- Dead)
  else (
    (match orig_status with
     | Alive ->
       [%log.global.debug "Was an alive region, now is zombie"];
       State.incr_zombie_regions state
     | _ -> ());
    young_region.region.status <- Zombie)

and update_and_generalize_young_region ~state young_region =
  update_types ~state young_region;
  generalize_young_region ~state young_region

and update_and_generalize ~state (curr_region : Type.region_node) =
  [%log.global.debug
    "Begin generalization (pre scheduler)"
      (curr_region.id : Identifier.t)
      (state.generalization_tree : Generalization_tree.t)];
  [%log.global.debug "Running scheduler" (state.scheduler : Scheduler.t)];
  Scheduler.run state.scheduler;
  [%log.global.debug "Begin generalization" (curr_region.id : Identifier.t)];
  let young_region = Young_region.of_region_node curr_region in
  update_and_generalize_young_region ~state young_region;
  [%log.global.debug "End generalization" (curr_region.id : Identifier.t)]
;;

let create_scheme root region_node : Type.t Scheme.t = { root; region_node }
let exit_region ~curr_region root = create_scheme root (Some curr_region)

let force_generalization ~state region_node =
  Generalization_tree.generalize_region
    state.generalization_tree
    region_node
    ~f:(update_and_generalize ~state)
;;

let instantiate ~state ~curr_region ({ root; region_node } : Type.t Scheme.t) =
  [%log.global.debug
    "Generalization tree @ instantiation"
      (state.generalization_tree : Generalization_tree.t)];
  assert (Queue.is_empty state.scheduler.job_queue);
  (* Generalize the region (if necessary) *)
  Option.iter region_node ~f:(force_generalization ~state);
  (* Make the copy of the type *)
  let copies = Hashtbl.create (module Identifier) in
  let rec loop type_ =
    let structure = Type.structure type_ in
    match structure.status with
    | Instance _ | Partial { kind = Instance; _ } -> type_
    | _ ->
      (try Hashtbl.find_exn copies structure.id with
       | Not_found_s _ ->
         (* Register instance if partial *)
         let copy =
           match structure.status with
           | Partial { kind = Generic; instances; region_node } ->
             let guard = structure.id in
             let v =
               create_var
                 ~state
                 ~curr_region
                 ~guards:(Hash_set.of_list (module Identifier) [ guard ])
                 ()
             in
             Type.set_structure
               type_
               { structure with
                 status =
                   Partial
                     { kind = Generic; region_node; instances = (guard, v) :: instances }
               };
             v
           | _ -> create_var ~state ~curr_region ()
         in
         Hashtbl.set copies ~key:structure.id ~data:copy;
         Type.set_inner copy (S.Inner.map structure.inner ~f:loop);
         copy)
  in
  loop root
;;
