open! Import
open Ast_types
open Ast
open Constraint

module Convert = struct
  let rec core_type ~env (type_ : Ast.core_type) : Type.t Or_error.t =
    let open Or_error.Let_syntax in
    match type_ with
    | Type_var v ->
      let%map v =
        Env.find_type_var env v
        |> Or_error.of_option
             ~error:
               (Error.create_s [%message "Unbound type variable" (v : Type_var_name.t)])
      in
      Type.var v
    | Type_arrow (type1, type2) ->
      let%map type1 = core_type ~env type1
      and type2 = core_type ~env type2 in
      Type.(type1 @-> type2)
    | Type_tuple types ->
      let%map types = types |> List.map ~f:(core_type ~env) |> Or_error.all in
      Type.tuple types
    | Type_constr (arg_types, constr) ->
      let%map arg_types = arg_types |> List.map ~f:(core_type ~env) |> Or_error.all in
      Type.constr arg_types constr
  ;;

  let core_scheme ~(env : Env.t) ({ scheme_quantifiers; scheme_body } : Ast.core_scheme)
    : (Type.Var.t list * Type.t) Or_error.t
    =
    let open Or_error.Let_syntax in
    let env, quantifiers =
      List.fold_map scheme_quantifiers ~init:env ~f:(fun env type_var ->
        let ctype_var =
          Type.Var.create ~id_source:env.id_source ~name:(type_var :> string) ()
        in
        let env = Env.add_type_var env ~type_var ~ctype_var in
        env, ctype_var)
    in
    let%map body = core_type ~env scheme_body in
    quantifiers, body
  ;;
end

let infer_constant const =
  match const with
  | Const_int _ -> Predef.int
  | Const_bool _ -> Predef.bool
  | Const_unit -> Predef.unit
;;

let inst_constr ~(env : Env.t) constr_name constr_type' k =
  let open Or_error.Let_syntax in
  (* Lookup constructor *)
  let%bind { constructor_alphas; constructor_arg; constructor_type; constructor_name = _ }
    =
    Env.find_constr env constr_name
    |> Or_error.of_option ~error:(Error.create_s [%message "Unbound constructor"])
  in
  (* Bind [alphas] existentially *)
  let env, constr_vars =
    List.fold_map constructor_alphas ~init:env ~f:(fun env type_var ->
      let ctype_var =
        Type.Var.create ~id_source:env.id_source ~name:(type_var :> string) ()
      in
      Env.add_type_var env ~type_var ~ctype_var, ctype_var)
  in
  (* Convert [constructor_arg] and [constructor_type] *)
  let%bind constr_arg =
    match constructor_arg with
    | None -> return None
    | Some constr_arg -> Convert.core_type ~env constr_arg >>| Option.some
  in
  let%bind constr_type = Convert.core_type ~env constructor_type in
  let%bind result, c = k constr_arg in
  return (result, exists_many constr_vars @@ (constr_type' =~ constr_type &~ c))
;;

module Pattern = struct
  module Fragment = struct
    type t = { var_bindings : Type.t Var_name.Map.t } [@@deriving sexp_of]

    let empty = { var_bindings = Var_name.Map.empty }
    let singleton var type_ = { var_bindings = Var_name.Map.singleton var type_ }

    let extend t ~var ~type_ =
      { var_bindings = Map.set t.var_bindings ~key:var ~data:type_ }
    ;;

    let merge t1 t2 =
      { var_bindings =
          Map.merge_skewed t1.var_bindings t2.var_bindings ~combine:(fun ~key:_ _ b -> b)
      }
    ;;

    let to_alist t = Map.to_alist t.var_bindings
  end

  let exists' ~id_source f =
    let open Or_error.Let_syntax in
    let a = Type.Var.create ~id_source () in
    let%map result, c = f (Type.var a) in
    result, exists a c
  ;;

  let rec infer_pat ~env pat pat_type k =
    let open Or_error.Let_syntax in
    match pat with
    | Pat_any -> k (Fragment.empty, tt)
    | Pat_var x -> k (Fragment.singleton x pat_type, tt)
    | Pat_alias (pat, x) ->
      infer_pat ~env pat pat_type
      @@ fun (f, c) ->
      let f = Fragment.extend f ~var:x ~type_:pat_type in
      k (f, c)
    | Pat_const const -> k (Fragment.empty, pat_type =~ infer_constant const)
    | Pat_tuple pats ->
      infer_pats ~env pats
      @@ fun (f, pat_types, c) -> k (f, c &~ Type.(pat_type =~ tuple pat_types))
    | Pat_constr (constr, arg_pat) ->
      inst_constr ~env constr pat_type
      @@ fun arg_type ->
      (match arg_pat, arg_type with
       | Some arg_pat, Some arg_type -> infer_pat ~env arg_pat arg_type k
       | None, None -> k (Fragment.empty, tt)
       | _ ->
         Or_error.error_s
           [%message "Constructor argument mistmatch in pattern" (pat : Ast.pattern)])
    | Pat_annot (pat, annot) ->
      let%bind type_ = Convert.core_type ~env annot in
      infer_pat ~env pat pat_type @@ fun (f, c) -> k (f, pat_type =~ type_ &~ c)

  and infer_pats ~env pats k =
    match pats with
    | [] -> k (Fragment.empty, [], tt)
    | pat :: pats ->
      exists' ~id_source:env.id_source
      @@ fun pat_type ->
      infer_pat ~env pat pat_type
      @@ fun (f, c1) ->
      infer_pats ~env pats
      @@ fun (f', pat_types, c2) ->
      k (Fragment.merge f f', pat_type :: pat_types, c1 &~ c2)
  ;;
end

module Expression = struct
  let exists' ~id_source f =
    let open Or_error.Let_syntax in
    let a = Type.Var.create ~id_source () in
    let%map c = f (Type.var a) in
    exists a c
  ;;

  let bind_pat ~env pat pat_type ~in_ =
    let open Or_error.Let_syntax in
    (Pattern.infer_pat ~env pat pat_type
     @@ fun (fragment, c) ->
     let bindings =
       fragment
       |> Pattern.Fragment.to_alist
       |> List.map ~f:(fun (var, type_) ->
         var, Var.create ~id_source:env.id_source ~name:(var :> string) (), type_)
     in
     let env =
       List.fold bindings ~init:env ~f:(fun env (var, cvar, _) ->
         Env.add_var env ~var ~cvar)
     in
     let%map in_ = in_ env in
     ( ()
     , c
       &~ List.fold bindings ~init:in_ ~f:(fun in_ (_, var, type_) ->
         let_ var #= (mono_scheme type_) ~in_) ))
    >>| snd
  ;;

  let rec infer_exp ~(env : Env.t) exp exp_type =
    let open Or_error.Let_syntax in
    let id_source = env.id_source in
    match exp with
    | Exp_var var ->
      let%bind var =
        Env.find_var env var
        |> Or_error.of_option
             ~error:(Error.create_s [%message "Unbound variable" (var : Var_name.t)])
      in
      return @@ inst var exp_type
    | Exp_const const -> return @@ (exp_type =~ infer_constant const)
    | Exp_fun (pat, exp) ->
      exists' ~id_source
      @@ fun a1 ->
      exists' ~id_source
      @@ fun a2 ->
      let%map c = bind_pat ~env pat a1 ~in_:(fun env -> infer_exp ~env exp a2) in
      Type.(exp_type =~ a1 @-> a2) &~ c
    | Exp_app (exp1, exp2) ->
      exists' ~id_source
      @@ fun a1 ->
      exists' ~id_source
      @@ fun a2 ->
      let%bind c1 = infer_exp ~env exp1 a2 in
      let%map c2 = infer_exp ~env exp2 a1 in
      Type.(a2 =~ a1 @-> exp_type) &~ c1 &~ c2
    | Exp_let (value_binding, exp) ->
      infer_value_binding ~env value_binding @@ fun env -> infer_exp ~env exp exp_type
    | Exp_exists (type_vars, exp) ->
      let type_vars =
        List.map type_vars ~f:(fun type_var ->
          type_var, Type.Var.create ~id_source:env.id_source ~name:(type_var :> string) ())
      in
      let env =
        List.fold type_vars ~init:env ~f:(fun env (type_var, ctype_var) ->
          Env.add_type_var env ~type_var ~ctype_var)
      in
      let%map c = infer_exp ~env exp exp_type in
      exists_many (List.map type_vars ~f:snd) c
    | Exp_annot (exp, annot) ->
      let%bind annot = Convert.core_type ~env annot in
      let%map c = infer_exp ~env exp exp_type in
      exp_type =~ annot &~ c
    | Exp_tuple exps ->
      infer_exps ~env exps
      @@ fun (exp_types, c) ->
      let%map c = c in
      Type.(exp_type =~ tuple exp_types) &~ c
    | Exp_if_then_else (if_exp, then_exp, else_exp) ->
      let%bind c1 = infer_exp ~env if_exp Predef.bool in
      let%bind c2 = infer_exp ~env then_exp exp_type in
      let%map c3 = infer_exp ~env else_exp exp_type in
      c1 &~ c2 &~ c3
    | Exp_sequence (exp1, exp2) ->
      let%bind c1 = infer_exp ~env exp1 Predef.unit in
      let%map c2 = infer_exp ~env exp2 exp_type in
      c1 &~ c2
    | Exp_constr (constr, arg_exp) ->
      (inst_constr ~env constr exp_type
       @@ fun arg_type ->
       match arg_exp, arg_type with
       | Some arg_exp, Some arg_type -> infer_exp ~env arg_exp arg_type >>| fun c -> (), c
       | None, None -> return ((), tt)
       | _ ->
         Or_error.error_s
           [%message
             "Constructor argument mistmatch in expression" (exp : Ast.expression)])
      >>| snd
    | Exp_match (match_exp, cases) ->
      exists' ~id_source
      @@ fun match_exp_type ->
      let%bind c1 = infer_exp ~env match_exp match_exp_type in
      let%map c2 = infer_cases ~env cases ~lhs_type:match_exp_type ~rhs_type:exp_type in
      c1 &~ c2

  and infer_exps ~env exps k =
    let open Or_error.Let_syntax in
    match exps with
    | [] -> k ([], Ok tt)
    | exp :: exps ->
      exists' ~id_source:env.id_source
      @@ fun exp_type ->
      let c1 = infer_exp ~env exp exp_type in
      infer_exps ~env exps
      @@ fun (exp_types, c2) ->
      k
        ( exp_type :: exp_types
        , let%map c1 = c1
          and c2 = c2 in
          c1 &~ c2 )

  and infer_cases ~env cases ~lhs_type ~rhs_type =
    let open Or_error.Let_syntax in
    let%map cs =
      cases |> List.map ~f:(infer_case ~env ~lhs_type ~rhs_type) |> Or_error.all
    in
    all cs

  and infer_case ~env { case_lhs = pat; case_rhs = exp } ~lhs_type ~rhs_type =
    bind_pat ~env pat lhs_type ~in_:(fun env -> infer_exp ~env exp rhs_type)

  and infer_value_binding ~env { value_binding_var = var; value_binding_exp = exp } k =
    let open Or_error.Let_syntax in
    let exp_type_var = Type.Var.create ~id_source:env.id_source () in
    let exp_type = Type.var exp_type_var in
    let%bind c = infer_exp ~env exp exp_type in
    let cvar = Var.create ~id_source:env.id_source ~name:(var :> string) () in
    let env = Env.add_var env ~var ~cvar in
    let%map c' = k env in
    let_ cvar #= (poly_scheme ([ exp_type_var ] @. c @=> exp_type)) ~in_:c'
  ;;
end

module Structure = struct
  let infer_prim ~env value_desc k =
    let open Or_error.Let_syntax in
    let%bind quantifiers, type_ = Convert.core_scheme ~env value_desc.value_type in
    let cvar =
      Var.create ~id_source:env.id_source ~name:(value_desc.value_name :> string) ()
    in
    let env = Env.add_var env ~var:value_desc.value_name ~cvar in
    let%map c = k env in
    let_ cvar #= (poly_scheme (quantifiers @. tt @=> type_)) ~in_:c
  ;;

  let infer_type_decl ~env { type_decl_name; type_decl_params; type_decl_kind } =
    let type_kind =
      match type_decl_kind with
      | Type_decl_abstract -> Adt.Type_abstract
      | Type_decl_variant constr_decls ->
        let constructor_type =
          Type_constr
            ( List.map type_decl_params ~f:(fun type_var -> Type_var type_var)
            , type_decl_name )
        in
        let constr_decls =
          List.map constr_decls ~f:(fun { constructor_name; constructor_arg } ->
            { Adt.constructor_name
            ; constructor_alphas = type_decl_params
            ; constructor_type
            ; constructor_arg
            })
        in
        Type_variant constr_decls
    in
    Env.add_type_decl env { type_name = type_decl_name; type_kind }
  ;;

  let infer_type_decls ~env type_decls =
    List.fold type_decls ~init:env ~f:(fun env type_decl ->
      infer_type_decl ~env type_decl)
  ;;

  let rec infer_str ~env (str : Ast.structure) =
    match str with
    | [] -> Ok tt
    | Str_type type_decls :: str ->
      let env = infer_type_decls ~env type_decls in
      infer_str ~env str
    | Str_primitive value_desc :: str ->
      infer_prim ~env value_desc @@ fun env -> infer_str ~env str
    | Str_value value_binding :: str ->
      Expression.infer_value_binding ~env value_binding @@ fun env -> infer_str ~env str
  ;;
end
