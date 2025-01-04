open Core
open Mlsus_std
open Mlsus_constraint
module C = Constraint
module T = C.Type

let () =
  let open Async.Log.Global in
  For_testing.use_test_output ()
;;

let print_solve_result ?(log_level = `Info) cst =
  Async.Log.Global.set_level log_level;
  let result = Mlsus_constraint_solver.solve cst in
  match result with
  | Ok () -> print_s [%message "Constraint is satisfiable" (cst : Constraint.t)]
  | Error err ->
    print_s [%message "Constraint is unsatisfiable" (cst : Constraint.t) (err : Error.t)]
;;

let predef_ident =
  let id_source = Identifier.create_source () in 
  fun name -> T.Ident.create ~id_source ~name ()
;;

let tint_ident = predef_ident "int"
let tstring_ident = predef_ident "string"
let tint = T.constr [] tint_ident
let tstring = T.constr [] tstring_ident

let%expect_test "Cannot unsuspend undetermined" =
  let open C in
  let id_source = Identifier.create_source () in
  let a1 = T.Var.create ~id_source () in
  let cst = exists a1 @@ match_ a1 ~closure:[ a1 ] ~with_:(fun _ -> T.var a1 =~ tint) in
  print_solve_result cst;
  [%expect
    {|
    (num_zombie_regions(num_zombie_regions 1))
    ("Constraint is unsatisfiable"
     (cst
      (Exists ((id 0) (name Type.Var))
       (Match ((id 0) (name Type.Var)) ((type_vars (((id 0) (name Type.Var)))))
        <fun>)))
     (err
      ("Failed to solve constraint"
       (exn (Mlsus_constraint_solver__Generalization.Cannot_unsuspend_generic)))))
    |}]
;;

let%expect_test "Can unsuspend determined (pre)" =
  let open C in
  let id_source = Identifier.create_source () in
  let a1 = T.Var.create ~id_source () in
  let cst =
    exists a1
    @@ (T.(var a1 =~ tint)
        &~ match_ a1 ~closure:[] ~with_:(function
          | Constr ([], constr) when T.Ident.(constr = tint_ident) -> tt
          | _ -> ff))
  in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is satisfiable"
     (cst
      (Exists ((id 0) (name Type.Var))
       (Conj (Eq (Var ((id 0) (name Type.Var))) (Constr () ((id 0) (name int))))
        (Match ((id 0) (name Type.Var)) ((type_vars ())) <fun>)))))
    |}]
;;

let%expect_test "Can unsuspend determined (post)" =
  let open C in
  let id_source = Identifier.create_source () in
  let a1 = T.Var.create ~id_source () in
  let cst =
    exists a1
    @@ (match_ a1 ~closure:[] ~with_:(function
          | Constr ([], constr) when T.Ident.(constr = tint_ident) -> tt
          | _ -> ff)
        &~ T.(var a1 =~ tint))
  in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is satisfiable"
     (cst
      (Exists ((id 0) (name Type.Var))
       (Conj (Match ((id 0) (name Type.Var)) ((type_vars ())) <fun>)
        (Eq (Var ((id 0) (name Type.Var))) (Constr () ((id 0) (name int))))))))
    |}]
;;

let%expect_test "Cannot unsuspend circular dependencies" =
  let open C in
  let id_source = Identifier.create_source () in
  let a1 = T.Var.create ~id_source () in
  let a2 = T.Var.create ~id_source () in
  let cst =
    exists a1
    @@ exists a2
    @@ (match_ a1 ~closure:[ a2 ] ~with_:(fun _ -> T.var a2 =~ tint)
        &~ match_ a2 ~closure:[ a1 ] ~with_:(fun _ -> T.var a1 =~ tint))
  in
  print_solve_result cst;
  [%expect
    {|
    (num_zombie_regions(num_zombie_regions 1))
    ("Constraint is unsatisfiable"
     (cst
      (Exists ((id 0) (name Type.Var))
       (Exists ((id 1) (name Type.Var))
        (Conj
         (Match ((id 0) (name Type.Var)) ((type_vars (((id 1) (name Type.Var)))))
          <fun>)
         (Match ((id 1) (name Type.Var)) ((type_vars (((id 0) (name Type.Var)))))
          <fun>)))))
     (err
      ("Failed to solve constraint"
       (exn (Mlsus_constraint_solver__Generalization.Cannot_unsuspend_generic)))))
    |}]
;;

let%expect_test "Can unsuspend topological dependencies" =
  let open C in
  let id_source = Identifier.create_source () in
  let a1 = T.Var.create ~id_source () in
  let a2 = T.Var.create ~id_source () in
  let cst =
    exists a1
    @@ exists a2
    @@ (T.(var a1 =~ tint)
        &~ match_ a1 ~closure:[ a2 ] ~with_:(fun _ -> T.var a2 =~ tint)
        &~ match_ a2 ~closure:[] ~with_:(fun _ -> tt))
  in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is satisfiable"
     (cst
      (Exists ((id 0) (name Type.Var))
       (Exists ((id 1) (name Type.Var))
        (Conj
         (Conj
          (Eq (Var ((id 0) (name Type.Var))) (Constr () ((id 0) (name int))))
          (Match ((id 0) (name Type.Var))
           ((type_vars (((id 1) (name Type.Var))))) <fun>))
         (Match ((id 1) (name Type.Var)) ((type_vars ())) <fun>))))))
    |}]
;;

let%expect_test "No suspended matches results in normal generalization" =
  let open C in
  (* Example constraint is for the program:
     let id = fun x -> x in
     id 1
  *)
  let id_source = Identifier.create_source () in
  let a1 = T.Var.create ~id_source () in
  let a2 = T.Var.create ~id_source () in
  let a3 = T.Var.create ~id_source () in
  let a4 = T.Var.create ~id_source () in
  let a5 = T.Var.create ~id_source () in
  let a6 = T.Var.create ~id_source () in
  let xid = Var.create ~id_source () in
  let xx = Var.create ~id_source () in
  let cst =
    exists a1
    @@ let_
         xid
         #= (poly_scheme
               ([ a2 ]
                @. (exists a3
                    @@ exists a4
                    @@ (T.(var a2 =~ var a3 @-> var a4)
                        &~ let_ xx #= (mono_scheme (T.var a3)) ~in_:(inst xx (T.var a4)))
                   )
                @=> T.var a2))
         ~in_:
           (exists a5
            @@ exists a6
            @@ (inst xid (T.var a5)
                &~ T.(var a5 =~ var a6 @-> var a1)
                &~ T.(var a6 =~ tint)))
  in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is satisfiable"
     (cst
      (Exists ((id 0) (name Type.Var))
       (Let ((id 6) (name Constraint.Var))
        ((type_vars (((id 1) (name Type.Var))))
         (in_
          (Exists ((id 2) (name Type.Var))
           (Exists ((id 3) (name Type.Var))
            (Conj
             (Eq (Var ((id 1) (name Type.Var)))
              (Arrow (Var ((id 2) (name Type.Var)))
               (Var ((id 3) (name Type.Var)))))
             (Let ((id 7) (name Constraint.Var))
              ((type_vars ()) (in_ True) (type_ (Var ((id 2) (name Type.Var)))))
              (Instance ((id 7) (name Constraint.Var))
               (Var ((id 3) (name Type.Var)))))))))
         (type_ (Var ((id 1) (name Type.Var)))))
        (Exists ((id 4) (name Type.Var))
         (Exists ((id 5) (name Type.Var))
          (Conj
           (Conj
            (Instance ((id 6) (name Constraint.Var))
             (Var ((id 4) (name Type.Var))))
            (Eq (Var ((id 4) (name Type.Var)))
             (Arrow (Var ((id 5) (name Type.Var)))
              (Var ((id 0) (name Type.Var))))))
           (Eq (Var ((id 5) (name Type.Var))) (Constr () ((id 0) (name int)))))))))))
    |}]
;;

let%expect_test "Partial generic becomes instance" =
  let open C in
  let id_source = Identifier.create_source () in
  let a1 = T.Var.create ~id_source () in
  let a2 = T.Var.create ~id_source () in
  let a3 = T.Var.create ~id_source () in
  let x1 = Var.create ~id_source () in
  let cst =
    exists a1
    @@ exists a2
    @@ let_
         x1
         #= (poly_scheme
               ([ a3 ]
                @. match_ a1 ~closure:[ a3; a2 ] ~with_:(fun _ ->
                  T.(var a3 =~ var a2) &~ T.(var a2 =~ tint))
                @=> T.var a3))
         ~in_:(inst x1 tint &~ T.(var a1 =~ tstring))
  in
  print_solve_result cst;
  [%expect
    {|
    (num_zombie_regions(num_zombie_regions 1))
    ("Constraint is unsatisfiable"
     (cst
      (Exists ((id 0) (name Type.Var))
       (Exists ((id 1) (name Type.Var))
        (Let ((id 3) (name Constraint.Var))
         ((type_vars (((id 2) (name Type.Var))))
          (in_
           (Match ((id 0) (name Type.Var))
            ((type_vars (((id 1) (name Type.Var)) ((id 2) (name Type.Var)))))
            <fun>))
          (type_ (Var ((id 2) (name Type.Var)))))
         (Conj
          (Instance ((id 3) (name Constraint.Var))
           (Constr () ((id 0) (name int))))
          (Eq (Var ((id 0) (name Type.Var))) (Constr () ((id 1) (name string)))))))))
     (err
      ("Failed to solve constraint"
       (exn (Mlsus_constraint_solver__Generalization.Cannot_unsuspend_generic)))))
    |}]
;;

let%expect_test "Partial generic becomes generic" =
  let open C in
  let id_source = Identifier.create_source () in
  let a1 = T.Var.create ~id_source () in
  let a2 = T.Var.create ~id_source () in
  let a3 = T.Var.create ~id_source () in
  let x1 = Var.create ~id_source () in
  let cst =
    exists a1
    @@ let_
         x1
         #= (poly_scheme
               ([ a2 ]
                @. match_ a1 ~closure:[ a2 ] ~with_:(fun _ ->
                  exists a3 @@ T.(var a2 =~ var a3 @-> var a3))
                @=> T.var a2))
         ~in_:
           (inst x1 T.(tint @-> tint)
            &~ inst x1 T.(tstring @-> tstring)
            &~ T.(var a1 =~ tint))
  in
  print_solve_result cst;
  [%expect
    {|
    (num_zombie_regions(num_zombie_regions 2))
    ("Constraint is unsatisfiable"
     (cst
      (Exists ((id 0) (name Type.Var))
       (Let ((id 3) (name Constraint.Var))
        ((type_vars (((id 1) (name Type.Var))))
         (in_
          (Match ((id 0) (name Type.Var))
           ((type_vars (((id 1) (name Type.Var))))) <fun>))
         (type_ (Var ((id 1) (name Type.Var)))))
        (Conj
         (Conj
          (Instance ((id 3) (name Constraint.Var))
           (Arrow (Constr () ((id 0) (name int)))
            (Constr () ((id 0) (name int)))))
          (Instance ((id 3) (name Constraint.Var))
           (Arrow (Constr () ((id 1) (name string)))
            (Constr () ((id 1) (name string))))))
         (Eq (Var ((id 0) (name Type.Var))) (Constr () ((id 0) (name int))))))))
     (err
      ("Failed to solve constraint"
       (exn (Mlsus_constraint_solver__Generalization.Cannot_unsuspend_generic)))))
    |}]
;;
