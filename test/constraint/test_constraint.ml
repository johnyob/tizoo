open Core
open Tizoo_constraints
module C = Constraint
module T = C.Type

let print_solve_result cst =
  let result = C.solve cst in
  match result with
  | Ok () -> print_s [%message "Constraint is satisfiable" (cst : Constraint.t)]
  | Error err ->
    print_s [%message "Constraint is unsatisfiable" (cst : Constraint.t) (err : Error.t)]
;;

let tint = T.constr [] "int"
let tstring = T.constr [] "string"
let tarr t1 t2 = T.constr [ t1; t2 ] "->"

let%expect_test "Cannot unsuspend undetermined" =
  let open C in
  let a1 = T.Var.create () in
  let cst = exists a1 (match_ a1 ~closure:[ `T a1 ] ~with_:(fun _ -> T.var a1 =~ tint)) in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is unsatisfiable"
     (cst
      (Exists ((id 1) (name Type.Var))
       (Match ((id 1) (name Type.Var))
        ((type_vars (((id 1) (name Type.Var)))) (vars ())) <fun>)))
     (err
      ("Failed to solve constraint"
       (exn (Tizoo_constraints__Generalization.Cannot_unsuspend_generic)))))
    |}]
;;

let%expect_test "Can unsuspend determined" =
  let open C in
  let a1 = T.Var.create () in
  let cst =
    exists
      a1
      (T.var a1
       =~ tint
       &~ match_ a1 ~closure:[] ~with_:(function
         | Constr ([], "int") -> tt
         | _ -> ff))
  in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is satisfiable"
     (cst
      (Exists ((id 2) (name Type.Var))
       (Conj (Eq (Var ((id 2) (name Type.Var))) (Structure (Constr () int)))
        (Match ((id 2) (name Type.Var)) ((type_vars ()) (vars ())) <fun>)))))
    |}]
;;

let%expect_test "Cannot unsuspend circular dependencies" =
  let open C in
  let a1 = T.Var.create () in
  let a2 = T.Var.create () in
  let cst =
    exists
      a1
      (exists
         a2
         (match_ a1 ~closure:[ `T a2 ] ~with_:(fun _ -> T.var a2 =~ tint)
          &~ match_ a2 ~closure:[ `T a1 ] ~with_:(fun _ -> T.var a1 =~ tint)))
  in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is unsatisfiable"
     (cst
      (Exists ((id 3) (name Type.Var))
       (Exists ((id 4) (name Type.Var))
        (Conj
         (Match ((id 3) (name Type.Var))
          ((type_vars (((id 4) (name Type.Var)))) (vars ())) <fun>)
         (Match ((id 4) (name Type.Var))
          ((type_vars (((id 3) (name Type.Var)))) (vars ())) <fun>)))))
     (err
      ("Failed to solve constraint"
       (exn (Tizoo_constraints__Generalization.Cannot_unsuspend_generic)))))
    |}]
;;

let%expect_test "Can unsuspend topological dependencies" =
  let open C in
  let a1 = T.Var.create () in
  let a2 = T.Var.create () in
  let cst =
    exists
      a1
      (exists
         a2
         (T.var a1
          =~ tint
          &~ match_ a1 ~closure:[ `T a2 ] ~with_:(fun _ -> T.var a2 =~ tint)
          &~ match_ a2 ~closure:[] ~with_:(fun _ -> tt)))
  in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is satisfiable"
     (cst
      (Exists ((id 5) (name Type.Var))
       (Exists ((id 6) (name Type.Var))
        (Conj
         (Conj (Eq (Var ((id 5) (name Type.Var))) (Structure (Constr () int)))
          (Match ((id 5) (name Type.Var))
           ((type_vars (((id 6) (name Type.Var)))) (vars ())) <fun>))
         (Match ((id 6) (name Type.Var)) ((type_vars ()) (vars ())) <fun>))))))
    |}]
;;

let%expect_test "Can unsuspend with cvar in closure" =
  let open C in
  let a1 = T.Var.create () in
  let a2 = T.Var.create () in
  let x1 = Var.create () in
  let cst =
    exists
      a1
      (exists
         a2
         (let_
            x1 #= (mono_scheme (T.var a2))
            ~in_:
              (match_ a1 ~closure:[ `T a2; `C x1 ] ~with_:(fun _ -> inst x1 tstring)
               &~ (T.var a1 =~ tint))
          &~ (T.var a2 =~ tint)))
  in
  print_solve_result cst;
  [%expect
    {|
    ("Constraint is unsatisfiable"
     (cst
      (Exists ((id 7) (name Type.Var))
       (Exists ((id 8) (name Type.Var))
        (Conj
         (Let ((id 1) (name Constraint.Var))
          ((type_vars ()) (in_ True) (type_ (Var ((id 8) (name Type.Var)))))
          (Conj
           (Match ((id 7) (name Type.Var))
            ((type_vars (((id 8) (name Type.Var))))
             (vars (((id 1) (name Constraint.Var)))))
            <fun>)
           (Eq (Var ((id 7) (name Type.Var))) (Structure (Constr () int)))))
         (Eq (Var ((id 8) (name Type.Var))) (Structure (Constr () int)))))))
     (err
      ("Failed to solve constraint"
       (exn
        (Not_found_s ("Map.find_exn: not found" ((id 1) (name Constraint.Var))))))))
    |}]
;;

let%expect_test "Partial generic is still generic" =
  let open C in
  let a1 = T.Var.create () in
  let a2 = T.Var.create () in
  let a3 = T.Var.create () in
  let x1 = Var.create () in
  let cst =
    exists
      a1
      (let_
         x1
         #= (poly_scheme
               ([ a2 ]
                @. match_
                     a1
                     ~closure:[ `T a2 ]
                     ~with_:(fun _ -> exists a3 (T.var a2 =~ T.(var a3 @-> var a3)))
                @=> T.var a2))
         ~in_:
           (inst x1 T.(tint @-> tint)
            &~ inst x1 T.(tstring @-> tstring)
            &~ (T.var a1 =~ tint)))
  in
  print_solve_result cst;
  [%expect {|
    ("Constraint is unsatisfiable"
     (cst
      (Exists ((id 9) (name Type.Var))
       (Let ((id 2) (name Constraint.Var))
        ((type_vars (((id 10) (name Type.Var))))
         (in_
          (Match ((id 9) (name Type.Var))
           ((type_vars (((id 10) (name Type.Var)))) (vars ())) <fun>))
         (type_ (Var ((id 10) (name Type.Var)))))
        (Conj
         (Conj
          (Instance ((id 2) (name Constraint.Var))
           (Structure
            (Arrow (Structure (Constr () int)) (Structure (Constr () int)))))
          (Instance ((id 2) (name Constraint.Var))
           (Structure
            (Arrow (Structure (Constr () string)) (Structure (Constr () string))))))
         (Eq (Var ((id 9) (name Type.Var))) (Structure (Constr () int)))))))
     (err
      ("Failed to solve constraint"
       (exn (Not_found_s ("Hashtbl.find_exn: not found" 4))))))
    |}]
;;
