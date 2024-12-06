open Core
module C = Constraint
module G = Generalization
module Type = G.Type

module Env = struct
  type t =
    { type_vars : Type.t C.Type.Var.Map.t
    ; expr_vars : Type.scheme C.Var.Map.t
    }

  let empty = { type_vars = C.Type.Var.Map.empty; expr_vars = C.Var.Map.empty }

  let bind_type_var t ~var ~type_ =
    { t with type_vars = Map.set t.type_vars ~key:var ~data:type_ }
  ;;

  let bind_var t ~var ~type_ =
    { t with expr_vars = Map.set t.expr_vars ~key:var ~data:type_ }
  ;;

  let find_type_var t type_var = Map.find_exn t.type_vars type_var
  let find_var t expr_var = Map.find_exn t.expr_vars expr_var

  let of_gclosure (gclosure : _ G.Suspended_match.closure) ~(closure : C.Closure.t) =
    let type_vars =
      List.zip_exn (Set.to_list closure.type_vars) gclosure.variables
      |> C.Type.Var.Map.of_alist_exn
    in
    { empty with type_vars }
  ;;
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

module State = struct
  type t = { gstate : G.State.t }

  let create () = { gstate = G.State.create () }
  let enter_region { gstate } = { gstate = G.enter ~state:gstate }

  let exit_region { gstate } ?root () =
    let gstate, quantifier_insts, root = G.exit ~state:gstate ?root () in
    { gstate }, quantifier_insts, root
  ;;
end

let rec gtype_of_type : state:State.t -> env:Env.t -> C.Type.t -> G.Type.t =
  fun ~state ~env type_ ->
  let self = gtype_of_type ~state ~env in
  match type_ with
  | Var type_var -> Env.find_type_var env type_var
  | Structure former ->
    G.create_former ~state:state.gstate (Structure.Former.hmap former ~f:self)
;;

let exists ~(state : State.t) ~env ~type_var =
  Env.bind_type_var env ~var:type_var ~type_:(G.create_var ~state:state.gstate ())
;;

exception Unsatisfiable

let unify ~(state : State.t) gtype1 gtype2 =
  try G.unify ~state:state.gstate gtype1 gtype2 with
  | G.U.Unify _ -> raise Unsatisfiable
;;

let rec solve : state:State.t -> env:Env.t -> C.t -> State.t =
  fun ~state ~env cst ->
  let self ~state ?(env = env) cst = solve ~state ~env cst in
  match cst with
  | True -> state
  | False -> raise Unsatisfiable
  | Conj (cst1, cst2) ->
    let state = self ~state cst1 in
    let state = self ~state cst2 in
    state
  | Eq (type1, type2) ->
    let gtype1 = gtype_of_type ~state ~env type1 in
    let gtype2 = gtype_of_type ~state ~env type2 in
    unify ~state gtype1 gtype2;
    state
  | Let (var, scheme, in_) ->
    let state, gscheme = gscheme_of_scheme ~state ~env scheme in
    let env = Env.bind_var env ~var ~type_:gscheme in
    self ~state ~env in_
  | Instance (var, expected_type) ->
    let expected_gtype = gtype_of_type ~state ~env expected_type in
    let var_gscheme = Env.find_var env var in
    let actual_gtype = G.instantiate ~state:state.gstate var_gscheme in
    unify ~state actual_gtype expected_gtype;
    state
  | Exists (type_var, cst) ->
    let env = exists ~state ~env ~type_var in
    let state = self ~state ~env cst in
    state
  | Match (matchee, closure, f) ->
    let matchee = Env.find_type_var env matchee in
    let gclosure = gclosure_of_closure ~env closure in
    let case structure =
      let env = Env.of_gclosure gclosure ~closure in
      let scruintee, env = match_type ~env structure in
      let cst = f scruintee in
      ignore (solve ~state ~env cst : State.t);
    in
    G.suspend { matchee; closure = gclosure; case };
    state

and gclosure_of_closure ~env closure : _ G.Suspended_match.closure =
  let variables =
    closure.type_vars |> Set.to_list |> List.map ~f:(Env.find_type_var env)
  in
  { variables }

and gscheme_of_scheme ~state ~env { type_vars; in_; type_ } =
  let state = State.enter_region state in
  let env =
    List.fold type_vars ~init:env ~f:(fun env type_var -> exists ~state ~env ~type_var)
  in
  let type_ = gtype_of_type ~state ~env type_ in
  let state = solve ~state ~env in_ in
  let state, _, scheme = State.exit_region state ~root:type_ () in
  state, Option.value_exn ~here:[%here] scheme
;;

let solve : C.t -> unit Or_error.t =
  fun cst ->
  let state = State.create () in
  try
    let state = State.enter_region state in
    let state = solve ~state ~env:Env.empty cst in
    ignore (State.exit_region state () : State.t * Type.t list * Type.scheme option);
    Ok ()
  with
  | exn -> Or_error.error_s [%message "Failed to solve constraint" (exn : exn)]
;;