open! Import
module C = Constraint
module G = Generalization
module Type = G.Type
module State = G.State

module Env = struct
  type t =
    { type_vars : Type.t C.Type.Var.Map.t
    ; expr_vars : Type.scheme C.Var.Map.t
    ; curr_region : G.Type.region_node
    }
  [@@deriving sexp_of]

  let empty curr_region =
    { type_vars = C.Type.Var.Map.empty; expr_vars = C.Var.Map.empty; curr_region }
  ;;

  let bind_type_var t ~var ~type_ =
    { t with type_vars = Map.set t.type_vars ~key:var ~data:type_ }
  ;;

  let bind_var t ~var ~type_ =
    { t with expr_vars = Map.set t.expr_vars ~key:var ~data:type_ }
  ;;

  let find_type_var t type_var = Map.find_exn t.type_vars type_var
  let find_var t expr_var = Map.find_exn t.expr_vars expr_var

  let of_gclosure
    (gclosure : _ G.Suspended_match.closure)
    ~(matchee : Type.t)
    ~(closure : C.Closure.t)
    =
    let curr_region =
      Tree.unsafe_max_by_level
        (G.Type.region_exn ~here:[%here] matchee
         :: List.map gclosure.variables ~f:(Type.region_exn ~here:[%here]))
    in
    let type_vars =
      List.zip_exn (Set.to_list closure.type_vars) gclosure.variables
      |> C.Type.Var.Map.of_alist_exn
    in
    { (empty curr_region) with type_vars }
  ;;

  let enter_region ~state t = { t with curr_region = G.enter_region ~state t.curr_region }
  let exit_region ~state:_ t root = G.exit_region ~curr_region:t.curr_region root
end

let match_type : env:Env.t -> G.Type.t Structure.Former.t -> C.Scruintee.t * Env.t =
  fun ~env type_ ->
  match type_ with
  | Arrow (utype1, utype2) ->
    let type_var1 = C.Type.Var.create () in
    let type_var2 = C.Type.Var.create () in
    let env =
      env
      |> Env.bind_type_var ~var:type_var1 ~type_:utype1
      |> Env.bind_type_var ~var:type_var2 ~type_:utype2
    in
    Arrow (type_var1, type_var2), env
  | Constr (utypes, constr) ->
    let env, type_vars =
      List.fold_map utypes ~init:env ~f:(fun env utype ->
        let type_var = C.Type.Var.create () in
        let env = Env.bind_type_var env ~var:type_var ~type_:utype in
        env, type_var)
    in
    Constr (type_vars, constr), env
;;

let rec gtype_of_type : state:State.t -> env:Env.t -> C.Type.t -> G.Type.t =
  fun ~state ~env type_ ->
  let self = gtype_of_type ~state ~env in
  match type_ with
  | Var type_var -> Env.find_type_var env type_var
  | Structure former ->
    G.create_former
      ~state
      ~curr_region:env.curr_region
      (Structure.Former.map former ~f:self)
;;

let exists ~(state : State.t) ~env ~type_var =
  Env.bind_type_var
    env
    ~var:type_var
    ~type_:(G.create_var ~state ~curr_region:env.curr_region ())
;;

exception Unsatisfiable

let unify ~(state : State.t) ~(env : Env.t) gtype1 gtype2 =
  [%log.global.debug
    "Unify" (state : State.t) (env : Env.t) (gtype1 : Type.t) (gtype2 : Type.t)];
  try
    G.unify ~state ~curr_region:env.curr_region gtype1 gtype2;
    G.Scheduler.run state.scheduler
  with
  | G.Unify.Unify _ -> raise Unsatisfiable
;;

let rec solve : state:State.t -> env:Env.t -> C.t -> unit =
  fun ~state ~env cst ->
  [%log.global.debug "Solving constraint" (state : State.t) (env : Env.t) (cst : C.t)];
  let self ~state ?(env = env) cst = solve ~state ~env cst in
  match cst with
  | True -> ()
  | False -> raise Unsatisfiable
  | Conj (cst1, cst2) ->
    [%log.global.debug "Solving conj lhs"];
    self ~state cst1;
    [%log.global.debug "Solving conj rhs"];
    self ~state cst2
  | Eq (type1, type2) ->
    [%log.global.debug "Decoding type1" (type1 : C.Type.t)];
    let gtype1 = gtype_of_type ~state ~env type1 in
    [%log.global.debug "Decoded type1" (gtype1 : Type.t)];
    [%log.global.debug "Decoding type2" (type2 : C.Type.t)];
    let gtype2 = gtype_of_type ~state ~env type2 in
    [%log.global.debug "Decoded type2" (gtype2 : Type.t)];
    unify ~state ~env gtype1 gtype2
  | Let (var, scheme, in_) ->
    [%log.global.debug "Solving let scheme"];
    let gscheme = gscheme_of_scheme ~state ~env scheme in
    [%log.global.debug "Binding var to scheme" (var : C.Var.t) (gscheme : Type.scheme)];
    let env = Env.bind_var env ~var ~type_:gscheme in
    [%log.global.debug "Solving let body"];
    self ~state ~env in_
  | Instance (var, expected_type) ->
    [%log.global.debug "Decoding expected_type" (expected_type : C.Type.t)];
    let expected_gtype = gtype_of_type ~state ~env expected_type in
    [%log.global.debug "Decoded expected_type" (expected_gtype : Type.t)];
    let var_gscheme = Env.find_var env var in
    [%log.global.debug "Instantiating scheme" (var : C.Var.t) (var_gscheme : Type.scheme)];
    let actual_gtype = G.instantiate ~state ~curr_region:env.curr_region var_gscheme in
    [%log.global.debug "Scheme instance" (actual_gtype : Type.t)];
    unify ~state ~env actual_gtype expected_gtype
  | Exists (type_var, cst) ->
    [%log.global.debug "Binding unification for type_var" (type_var : C.Type.Var.t)];
    let env = exists ~state ~env ~type_var in
    [%log.global.debug "Updated env" (env : Env.t)];
    [%log.global.debug "Solving exist body"];
    self ~state ~env cst
  | Match (matchee, closure, f) ->
    let guard = Identifier.create state.id_source in
    let matchee = Env.find_type_var env matchee in
    [%log.global.debug "Matchee type" (matchee : Type.t)];
    let gclosure = gclosure_of_closure ~env closure in
    [%log.global.debug
      "Closure of suspended match" (gclosure : Type.t G.Suspended_match.closure)];
    let case structure =
      [%log.global.debug "Entered match handler" (structure : Type.t Structure.Former.t)];
      (* Enter region and construct env *)
      let env = Env.of_gclosure gclosure ~closure ~matchee in
      G.visit_region ~state env.curr_region;
      [%log.global.debug "Handler env" (env : Env.t)];
      [%log.global.debug "Handler state" (state : State.t)];
      (* Solve *)
      let scruintee, env = match_type ~env structure in
      [%log.global.debug
        "Scrutinee and updated env" (scruintee : C.Scruintee.t) (env : Env.t)];
      let cst = f scruintee in
      [%log.global.debug "Generated constraint from case" (cst : C.t)];
      solve ~state ~env cst;
      [%log.global.debug "Solved generated constraint" (cst : C.t)];
      (* Update any closure variables *)
      List.iter gclosure.variables ~f:(fun type_ ->
        let open G in
        remove_guard ~state type_ guard);
      [%log.global.debug "Exiting case region"]
    in
    [%log.global.debug "Suspending match..."];
    G.suspend ~state ~guard { matchee; closure = gclosure; case }

and gclosure_of_closure ~env closure : _ G.Suspended_match.closure =
  let variables =
    closure.type_vars |> Set.to_list |> List.map ~f:(Env.find_type_var env)
  in
  { variables }

and gscheme_of_scheme ~state ~env { type_vars; in_; type_ } =
  let env = Env.enter_region ~state env in
  [%log.global.debug "Entered new region" (env : Env.t)];
  let env =
    List.fold type_vars ~init:env ~f:(fun env type_var -> exists ~state ~env ~type_var)
  in
  [%log.global.debug "Bound type vars" (type_vars : C.Type.Var.t list) (env : Env.t)];
  let type_ = gtype_of_type ~state ~env type_ in
  [%log.global.debug "Solving scheme's constraint"];
  solve ~state ~env in_;
  [%log.global.debug "Type of scheme" (type_ : Type.t)];
  let scheme = Env.exit_region ~state env type_ in
  [%log.global.debug "Exiting region" (scheme : Type.scheme)];
  scheme
;;

let solve : C.t -> unit Or_error.t =
  fun cst ->
  try
    let state = State.create () in
    let env = Env.empty (G.root_region ~state) in
    [%log.global.debug "Initial env and state" (state : State.t) (env : Env.t)];
    solve ~state ~env cst;
    [%log.global.debug "State" (state : State.t)];
    [%log.global.debug "Generalizing root region" (env.curr_region : Type.region_node)];
    G.force_generalization ~state env.curr_region;
    [%log.global.debug "Generalized root region" (env.curr_region : Type.region_node)];
    [%log.global.debug "End state" (state : State.t)];
    if state.num_zombie_regions > 0
    then (
      [%log.global.error "num_zombie_regions" (state.num_zombie_regions : int)];
      raise G.Cannot_unsuspend_generic);
    Ok ()
  with
  | exn -> Or_error.error_s [%message "Failed to solve constraint" (exn : exn)]
;;
