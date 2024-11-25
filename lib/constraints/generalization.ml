open Core

module Level = struct
  module T = struct
    type t = int [@@deriving sexp, equal, compare]
  end

  include T
  include Comparable.Make (T)

  let init = -1
  let enter t = t + 1
  let exit t = t - 1
end

module Structure = struct
  module Ml = Structure.Ml
  module Inner = Structure.First_order (Ml)

  type 'a t =
    { id : int
    ; level : Level.t
    ; is_generic : bool
    ; inner : 'a Inner.t
    }
  [@@deriving sexp_of]

  type 'a ctx =
    { next_id : unit -> int
    ; set_region : 'a -> unit
    }

  let of_inner ~id inner = { id; level = Level.init; is_generic = false; inner }
  let map t ~f = { t with inner = Inner.map t.inner ~f }
  let iter t ~f = Inner.iter t.inner ~f
  let fold t ~f ~init = Inner.fold t.inner ~f ~init

  exception Cannot_merge = Inner.Cannot_merge

  let merge ~ctx ~create ~unify t1 t2 =
    let create inner =
      let type_ = create (of_inner ~id:(ctx.next_id ()) inner) in
      ctx.set_region type_;
      type_
    in
    let level = min t1.level t2.level in
    let is_generic = false in
    let inner = Inner.merge ~ctx:() ~create ~unify t1.inner t2.inner in
    { id = t1.id; level; is_generic; inner }
  ;;
end

module U = Tizoo_unifier.Unifier.Make (Structure)

module Type = struct
  include U.Type

  let inner t = (structure t).inner

  let set_inner t inner =
    let structure = structure t in
    set_structure t { structure with inner }
  ;;

  let id t = (structure t).id
  let level t = (structure t).level

  let set_level t level =
    let structure = structure t in
    set_structure t { structure with level }
  ;;

  let is_generic t = (structure t).is_generic

  let update_level t level =
    let structure = structure t in
    if level < structure.level then set_structure t { structure with level }
  ;;

  let is_generic_at t l = is_generic t && level t = l

  let generalize t =
    let structure = structure t in
    set_structure t { structure with is_generic = true }
  ;;
end

module Region = struct
  type t =
    { types : Type.t list
    ; wait_queue : wait_queue
    }

  and suspended_match =
    { scruintee : Type.t
    ; closure : closure
    ; f : closure -> Type.t Structure.Ml.t -> unit
    }

  and closure =
    { vars : Type.t list
    ; schemes : scheme list
    }

  and scheme =
    { root : Type.t
    ; partially_suspended : wait_queue
    ; level : Level.t
    }

  and wait_queue = suspended_match list

  let empty = { types = []; wait_queue = [] }
  let add_type t type_ = { t with types = type_ :: t.types }
  let wait_queue t = t.wait_queue
  let set_wait_queue t wait_queue = { t with wait_queue }

  let enqueue_suspended_match t suspended_match =
    { t with wait_queue = t.wait_queue @ [ suspended_match ] }
  ;;

  let dequeue_suspended_match t =
    match t.wait_queue with
    | [] -> None
    | suspended_match :: wait_queue -> Some (suspended_match, { t with wait_queue })
  ;;
end

module Q = Queue

module State = struct
  type t =
    { mutable next_id : int
    ; mutable current_level : Level.t
    ; regions : Region.t Dynarray.t
    }

  let next_id t =
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    id
  ;;

  let create () : t =
    { current_level = Level.init; regions = Dynarray.create (); next_id = 0 }
  ;;

  let region_at_level t level = Dynarray.get t.regions level

  let update_region_at_level t level ~f =
    let region = region_at_level t level in
    Dynarray.set t.regions level (f region)
  ;;

  let set_type_level t type_ level =
    Type.set_level type_ level;
    update_region_at_level t level ~f:(fun region -> Region.add_type region type_)
  ;;

  let suspend_at_level t suspended_match level =
    update_region_at_level t level ~f:(fun region ->
      Region.enqueue_suspended_match region suspended_match)
  ;;

  let unsuspend_at_level t level =
    let result = ref None in
    update_region_at_level t level ~f:(fun region ->
      match Region.dequeue_suspended_match region with
      | None -> region
      | Some (sm, region) ->
        result := Some sm;
        region);
    !result
  ;;

  let wait_queue_at_level t level =
    let region = region_at_level t level in
    Region.wait_queue region
  ;;

  let set_wait_queue_at_level t level wait_queue =
    update_region_at_level t level ~f:(fun region ->
      Region.set_wait_queue region wait_queue)
  ;;

  let to_ctx t : Type.t Structure.ctx =
    { next_id = (fun () -> next_id t)
    ; set_region = (fun type_ -> set_type_level t type_ t.current_level)
    }
  ;;
end

open State

let enter ~state =
  state.current_level <- Level.enter state.current_level;
  Dynarray.add_last state.regions Region.empty
;;

let create_type ~state inner =
  let type_ = Type.create (Structure.of_inner ~id:(State.next_id state) inner) in
  State.set_type_level state type_ state.current_level;
  type_
;;

let create_var ~state = create_type ~state Var
let create_former ~state ml = create_type ~state (Structure ml)

let suspend ~state suspended_match =
  State.suspend_at_level state suspended_match state.current_level
;;

let unify ~state type1 type2 = U.unify ~ctx:(State.to_ctx state) type1 type2

type scheme = Region.scheme

type young_region =
  { region : Region.t
  ; is_young : Type.t -> bool
  }

let young_region ~state : young_region =
  let region = Dynarray.get state.regions state.current_level in
  let is_young =
    let set = Hash_set.of_list (module Int) (region.types |> List.map ~f:Type.id) in
    fun type_ -> Hash_set.mem set (Type.id type_)
  in
  { region; is_young }
;;

let update_type_levels young_region =
  let visited = Int.Hash_set.create () in
  let rec loop type_ level =
    let id = Type.id type_ in
    if not (Hash_set.mem visited id)
    then (
      Hash_set.add visited id;
      Type.update_level type_ level;
      if young_region.is_young type_
      then Type.structure type_ |> Structure.iter ~f:(fun type_ -> loop type_ level))
  in
  List.iter young_region.region.types ~f:(fun type_ -> loop type_ (Type.level type_))
;;

exception Cannot_unsuspend of Type.t

let update_suspended_levels ~state =
  let rec loop partially_suspended =
    match State.unsuspend_at_level state state.current_level with
    | Some ({ scruintee; closure; f = _ } as suspended_match) ->
      let scruintee_level = Type.level scruintee in
      if scruintee_level = state.current_level
      then raise (Cannot_unsuspend scruintee)
      else (
        let level =
          List.fold
            ~init:scruintee_level
            ~f:(fun level type_ -> Level.max level (Type.level type_))
            closure.vars
        in
        if level = state.current_level
        then loop (suspended_match :: partially_suspended)
        else State.suspend_at_level state suspended_match level)
    | None -> State.set_wait_queue_at_level state state.current_level partially_suspended
  in
  loop []
;;

let generalize ~state young_region =
  List.iter young_region.region.types ~f:(fun type_ ->
    let level = Type.level type_ in
    if level < state.current_level
    then State.set_type_level state type_ level
    else Type.generalize type_);
  let generalizable =
    List.filter young_region.region.types ~f:(fun type_ ->
      Type.is_generic_at type_ state.current_level
      &&
      match Type.inner type_ with
      | Var -> true
      | _ -> false)
  in
  generalizable
;;

let unsuspend ~state =
  let rec loop : work_done:bool -> still_suspended:Region.wait_queue -> unit =
    fun ~work_done ~still_suspended ->
    match State.unsuspend_at_level state state.current_level with
    | Some ({ scruintee; closure; f } as suspended_match) ->
      (match Type.inner scruintee with
       | Var -> loop ~work_done ~still_suspended:(suspended_match :: still_suspended)
       | Structure structure ->
         f closure structure;
         loop ~work_done:true ~still_suspended)
    | None ->
      State.set_wait_queue_at_level state state.current_level still_suspended;
      if work_done then loop ~work_done:false ~still_suspended:[]
  in
  loop ~work_done:false ~still_suspended:[]
;;

let exit ~state ~types ?(do_unsuspend = false) () =
  if do_unsuspend then unsuspend ~state;
  let young_region = young_region ~state in
  update_type_levels young_region;
  update_suspended_levels ~state;
  let generalizable = generalize ~state young_region in
  let create_scheme =
    let level = state.current_level in
    let partially_suspended = State.wait_queue_at_level state level in
    fun root : scheme -> { root; partially_suspended; level }
  in
  ignore (Dynarray.pop_last state.regions : Region.t);
  state.current_level <- Level.exit state.current_level;
  generalizable, List.map ~f:create_scheme types
;;

let mono_scheme root : scheme =
  { root; partially_suspended = []; level = Type.level root + 1 }
;;

let instantiate ~state ({ root; partially_suspended; level } : scheme) =
  let copied : (int, Type.t) Hashtbl.t = Int.Table.create () in
  let instance_variables = ref [] in
  let rec copy type_ =
    if not (Type.is_generic type_)
    then type_
    else (
      let id = Type.id type_ in
      try Hashtbl.find_exn copied id with
      | Not_found_s _ ->
        let new_type = create_var ~state in
        Hashtbl.set copied ~key:id ~data:new_type;
        (match Type.inner type_ with
         | Var ->
           if Type.level type_ = level
           then instance_variables := new_type :: !instance_variables
         | Structure ml ->
           Type.set_inner new_type (Structure (Structure.Ml.map ml ~f:copy)));
        new_type)
  in
  let rec contains_copied type_ =
    if not (Type.is_generic type_)
    then false
    else
      Hashtbl.mem copied (Type.id type_)
      || Structure.Inner.fold (Type.inner type_) ~init:false ~f:(fun type_ acc ->
        acc || contains_copied type_)
  in
  let copy_scheme (scheme : scheme) =
    enter ~state;
    let scheme_copied : (int, Type.t) Hashtbl.t = Int.Table.create () in
    let rec loop type_ =
      if not (Type.is_generic type_)
      then type_
      else (
        let id = Type.id type_ in
        (* If we've copied this as part of instantiation, then use that *)
        try Hashtbl.find_exn copied id with
        | Not_found_s _ ->
          (* Otherwise duplicate, these will generalized *)
          (try Hashtbl.find_exn scheme_copied id with
           | Not_found_s _ ->
             let new_type = create_var ~state in
             Hashtbl.set scheme_copied ~key:id ~data:new_type;
             (match Type.inner type_ with
              | Var -> ()
              | Structure ml ->
                Type.set_inner new_type (Structure (Structure.Ml.map ml ~f:loop)));
             new_type))
    in
    let root = loop scheme.root in
    (* By default, exit will not unsuspend the suspended matches *)
    let _, schemes = exit ~state ~types:[ root ] () in
    List.hd_exn schemes
  in
  let root = copy root in
  let copy_closure (closure : Region.closure) : Region.closure =
    let vars =
      List.map closure.vars ~f:(fun type_ ->
        assert ((not (Type.is_generic type_)) || Hashtbl.mem copied (Type.id type_));
        copy type_)
    in
    let schemes =
      List.map closure.schemes ~f:(fun scheme ->
        if not (Type.is_generic scheme.root)
        then scheme
        else if contains_copied scheme.root
        then copy_scheme scheme
        else scheme)
    in
    { vars; schemes }
  in
  List.iter partially_suspended ~f:(fun suspended_match ->
    let copied_closure = copy_closure suspended_match.closure in
    State.suspend_at_level
      state
      { suspended_match with closure = copied_closure }
      state.current_level);
  !instance_variables, root
;;
