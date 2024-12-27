open! Import
module F = Structure.Former

module Region = struct
  (** The [Generalization] module manages the generalisation of graphical types.

      Each type belongs to a 'region', which indicates where those types are
      existentially bound in the solver's stack. *)
  type 'a t = { mutable types : 'a list } [@@deriving sexp_of]

  let create () = { types = [] }
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

module Status = struct
  type 'a t =
    | Instance of 'a Region.Tree.sexp_identifier_node
    | Generic
  [@@deriving sexp_of]

  let set_region t rn =
    match t with
    | Instance _ -> Instance rn
    | Generic -> Generic
  ;;

  let region t =
    match t with
    | Instance rn -> Some rn
    | Generic -> None
  ;;

  let merge t1 t2 =
    match t1, t2 with
    | Generic, _ | _, Generic -> assert false
    | Instance rn1, Instance rn2 -> Instance (Tree.nearest_common_ancestor rn1 rn2)
  ;;

  let of_region_node region_node = Instance region_node
end

module S = struct
  module Inner = Structure.First_order (F)

  type 'a t =
    { id : Identifier.t
    ; inner : 'a Inner.t
    ; status : 'a Status.t
    }
  [@@deriving sexp_of]

  let create ~id_source ~region_node inner =
    { id = Identifier.create id_source
    ; status = Status.of_region_node region_node
    ; inner
    }
  ;;

  let generalize t =
    match t.status with
    | Generic -> assert false
    | Instance _ -> { t with status = Generic }
  ;;

  type 'a ctx =
    { id_source : Identifier.source
    ; curr_region : 'a Region.Tree.node
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
    let status = Status.merge t1.status t2.status in
    let inner =
      Inner.merge ~ctx:ctx.super ~create ~unify ~type1 ~type2 t1.inner t2.inner
    in
    { id = t1.id; status; inner }
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
end

module Generalization_tree : sig
  (** Generalization can be performed lazily at instantiation. A region [rn] may
      be generalized provided all of the descendants are generalized. We represent
      this constraint as a tree of regions that need to be generalized,
      a {e generalization_tree}. Visiting a region signals that a region must be
      generalized at some point in the future.

      Note that the above implies that when generalizing the root region, all
      regions must be generalized. *)
  type t [@@deriving sexp_of]

  (** [create ()] returns an empty generalization tree. *)
  val create : unit -> t

  (** [visit_region t rn] visits a region [rn], marking it for generalization in
      the future. *)
  val visit_region : t -> Type.region_node -> unit

  (** [generalize_region t rn ~f] generalizes [rn] (and all of its decsendants that are
      to be generalized). [f rn'] is called for each generalizable region [rn'].

      Safety: [f rn] may update [t], but only the ancestors of [rn] stored in [t] *)
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
    ; generalization_tree : Generalization_tree.t
    }
  [@@deriving sexp_of]

  let create () =
    { id_source = Identifier.create_source ()
    ; generalization_tree = Generalization_tree.create ()
    }
  ;;
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

let create_type ~state ~curr_region inner =
  let type_ =
    Type.create (S.create ~id_source:state.id_source ~region_node:curr_region inner)
  in
  Region.(register_type (Tree.region curr_region) type_);
  type_
;;

let create_var ~state ~curr_region () = create_type ~state ~curr_region Var

let create_former ~state ~curr_region former =
  create_type ~state ~curr_region (Structure former)
;;

let unify ~state ~curr_region type1 type2 =
  let unifier_ctx : _ S.ctx = { id_source = state.id_source; curr_region; super = () } in
  Unify.unify ~ctx:unifier_ctx type1 type2
;;

let update_types ~state (young_region : Young_region.t) =
  [%log.global.debug "Updating types" (young_region : Young_region.t)];
  let visited = Hash_set.create (module Identifier) in
  let rec loop type_ r =
    [%log.global.debug "Visiting" (type_ : Type.t) (r : Type.sexp_identifier_region_node)];
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
        Type.structure type_ |> S.iter ~f:(fun type_ -> loop type_ r)))
  in
  List.iter young_region.region.types ~f:(fun type_ ->
    loop type_ (Type.region_exn ~here:[%here] type_))
;;

let generalize_young_region ~state (young_region : Young_region.t) =
  [%log.global.debug "Generalizing young region" (young_region : Young_region.t)];
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
         true)))
  in
  [%log.global.debug "Generics for young region" (generics : Type.t list)];
  young_region.region.types <- [];
  [%log.global.debug "Updated region" (young_region.region : Type.region)]
;;

let update_and_generalize_young_region ~state young_region =
  update_types ~state young_region;
  generalize_young_region ~state young_region
;;

let update_and_generalize ~state (curr_region : Type.region_node) =
  [%log.global.debug "Begin generalization" (curr_region.id : Identifier.t)];
  let young_region = Young_region.of_region_node curr_region in
  update_and_generalize_young_region ~state young_region;
  [%log.global.debug "End generalization" (curr_region.id : Identifier.t)]
;;

let create_scheme root region_node : Type.t Scheme.t =
  { root; region_node = Some region_node }
;;

let force_generalization ~state region_node =
  Generalization_tree.generalize_region
    state.generalization_tree
    region_node
    ~f:(update_and_generalize ~state)
;;

let exit_region ~curr_region root = create_scheme root curr_region

let instantiate ~state ~curr_region ({ root; region_node } : Type.t Scheme.t) =
  [%log.global.debug
    "Generalization tree @ instantiation"
      (state.generalization_tree : Generalization_tree.t)];
  (* Generalize the region (if necessary) *)
  Option.iter region_node ~f:(force_generalization ~state);
  (* Make the copy of the type *)
  let copies = Hashtbl.create (module Identifier) in
  let rec loop type_ =
    let structure = Type.structure type_ in
    match structure.status with
    | Instance _ -> type_
    | _ ->
      (try Hashtbl.find_exn copies structure.id with
       | Not_found_s _ ->
         let copy = create_var ~state ~curr_region () in
         Hashtbl.set copies ~key:structure.id ~data:copy;
         Type.set_inner copy (S.Inner.map structure.inner ~f:loop);
         copy)
  in
  loop root
;;
