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
    ("Suspend on"
     (suspended_match.matchee
      (Root
       ((rank 0)
        (value
         ((structure
           ((id 2) (guards ())
            (inner (Var (Empty_one_or_more_handlers (((run <fun>))))))
            (status (Instance <opaque>)))))))))
     (guard 3))
    ("Generalizing region"
     (curr_region
      ((region
        ((status Alive)
         (types
          ((Root
            ((rank 0)
             (value
              ((structure
                ((id 2) (guards (3))
                 (inner
                  (Var
                   (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                 (status (Instance <opaque>))))))))))))
       (node
        ((id 1)
         (parent
          (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
         (level 1)
         (region
          ((status Alive)
           (types
            ((Root
              ((rank 0)
               (value
                ((structure
                  ((id 2) (guards (3))
                   (inner
                    (Var
                     (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                   (status (Instance <opaque>))))))))))))))
       (path
        ((dst
          ((id 1)
           (parent
            (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
           (level 1)
           (region
            ((status Alive)
             (types
              ((Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 2) (guards (3))
                     (inner
                      (Var
                       (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                     (status (Instance <opaque>))))))))))))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    Type is made partial generic
    (id (id 3))
    Generaizing alive region into zombie region
    ("Zombie region"
     (partial_generics
      ((Root
        ((rank 0)
         (value
          ((structure
            ((id 2) (guards (3))
             (inner
              (Var (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
             (status (Partial_generic (region_node <opaque>) (instances ()))))))))))))
    (num_zombie_regions (!G.num_zombie_regions 1))
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
    ("Generalizing region"
     (curr_region
      ((region
        ((status Alive)
         (types
          ((Root
            ((rank 1)
             (value
              ((structure
                ((id 2) (guards ()) (inner (Structure (Constr () int)))
                 (status (Instance <opaque>))))))))
           (Inner
            (Root
             ((rank 1)
              (value
               ((structure
                 ((id 2) (guards ()) (inner (Structure (Constr () int)))
                  (status (Instance <opaque>)))))))))))))
       (node
        ((id 1)
         (parent
          (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
         (level 1)
         (region
          ((status Alive)
           (types
            ((Root
              ((rank 1)
               (value
                ((structure
                  ((id 2) (guards ()) (inner (Structure (Constr () int)))
                   (status (Instance <opaque>))))))))
             (Inner
              (Root
               ((rank 1)
                (value
                 ((structure
                   ((id 2) (guards ()) (inner (Structure (Constr () int)))
                    (status (Instance <opaque>)))))))))))))))
       (path
        ((dst
          ((id 1)
           (parent
            (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
           (level 1)
           (region
            ((status Alive)
             (types
              ((Root
                ((rank 1)
                 (value
                  ((structure
                    ((id 2) (guards ()) (inner (Structure (Constr () int)))
                     (status (Instance <opaque>))))))))
               (Inner
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 2) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>)))))))))))))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    Type made generic
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
    ("Suspend on"
     (suspended_match.matchee
      (Root
       ((rank 0)
        (value
         ((structure
           ((id 2) (guards ())
            (inner (Var (Empty_one_or_more_handlers (((run <fun>))))))
            (status (Instance <opaque>)))))))))
     (guard 4))
    ("Suspend on"
     (suspended_match.matchee
      (Root
       ((rank 0)
        (value
         ((structure
           ((id 3) (guards (4))
            (inner (Var (Empty_one_or_more_handlers (((run <fun>))))))
            (status (Instance <opaque>)))))))))
     (guard 5))
    ("Generalizing region"
     (curr_region
      ((region
        ((status Alive)
         (types
          ((Root
            ((rank 0)
             (value
              ((structure
                ((id 3) (guards (4))
                 (inner
                  (Var
                   (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                 (status (Instance <opaque>))))))))
           (Root
            ((rank 0)
             (value
              ((structure
                ((id 2) (guards (5))
                 (inner
                  (Var
                   (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                 (status (Instance <opaque>))))))))))))
       (node
        ((id 1)
         (parent
          (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
         (level 1)
         (region
          ((status Alive)
           (types
            ((Root
              ((rank 0)
               (value
                ((structure
                  ((id 3) (guards (4))
                   (inner
                    (Var
                     (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                   (status (Instance <opaque>))))))))
             (Root
              ((rank 0)
               (value
                ((structure
                  ((id 2) (guards (5))
                   (inner
                    (Var
                     (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                   (status (Instance <opaque>))))))))))))))
       (path
        ((dst
          ((id 1)
           (parent
            (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
           (level 1)
           (region
            ((status Alive)
             (types
              ((Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 3) (guards (4))
                     (inner
                      (Var
                       (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                     (status (Instance <opaque>))))))))
               (Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 2) (guards (5))
                     (inner
                      (Var
                       (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
                     (status (Instance <opaque>))))))))))))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    Type is made partial generic
    (id (id 4))
    Type is made partial generic
    (id (id 5))
    Generaizing alive region into zombie region
    ("Zombie region"
     (partial_generics
      ((Root
        ((rank 0)
         (value
          ((structure
            ((id 3) (guards (4))
             (inner
              (Var (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
             (status (Partial_generic (region_node <opaque>) (instances ())))))))))
       (Root
        ((rank 0)
         (value
          ((structure
            ((id 2) (guards (5))
             (inner
              (Var (Empty_one_or_more_handlers (((run <fun>)) ((run <fun>))))))
             (status (Partial_generic (region_node <opaque>) (instances ()))))))))))))
    (num_zombie_regions (!G.num_zombie_regions 1))
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
    ("Generalizing region"
     (curr_region
      ((region
        ((status Alive)
         (types
          ((Root
            ((rank 1)
             (value
              ((structure
                ((id 3) (guards ()) (inner (Structure (Constr () int)))
                 (status (Instance <opaque>))))))))
           (Root
            ((rank 1)
             (value
              ((structure
                ((id 2) (guards ()) (inner (Structure (Constr () int)))
                 (status (Instance <opaque>))))))))
           (Inner
            (Root
             ((rank 1)
              (value
               ((structure
                 ((id 3) (guards ()) (inner (Structure (Constr () int)))
                  (status (Instance <opaque>)))))))))
           (Inner
            (Root
             ((rank 1)
              (value
               ((structure
                 ((id 2) (guards ()) (inner (Structure (Constr () int)))
                  (status (Instance <opaque>)))))))))))))
       (node
        ((id 1)
         (parent
          (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
         (level 1)
         (region
          ((status Alive)
           (types
            ((Root
              ((rank 1)
               (value
                ((structure
                  ((id 3) (guards ()) (inner (Structure (Constr () int)))
                   (status (Instance <opaque>))))))))
             (Root
              ((rank 1)
               (value
                ((structure
                  ((id 2) (guards ()) (inner (Structure (Constr () int)))
                   (status (Instance <opaque>))))))))
             (Inner
              (Root
               ((rank 1)
                (value
                 ((structure
                   ((id 3) (guards ()) (inner (Structure (Constr () int)))
                    (status (Instance <opaque>)))))))))
             (Inner
              (Root
               ((rank 1)
                (value
                 ((structure
                   ((id 2) (guards ()) (inner (Structure (Constr () int)))
                    (status (Instance <opaque>)))))))))))))))
       (path
        ((dst
          ((id 1)
           (parent
            (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
           (level 1)
           (region
            ((status Alive)
             (types
              ((Root
                ((rank 1)
                 (value
                  ((structure
                    ((id 3) (guards ()) (inner (Structure (Constr () int)))
                     (status (Instance <opaque>))))))))
               (Root
                ((rank 1)
                 (value
                  ((structure
                    ((id 2) (guards ()) (inner (Structure (Constr () int)))
                     (status (Instance <opaque>))))))))
               (Inner
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 3) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>)))))))))
               (Inner
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 2) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>)))))))))))))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    Type made generic
    Type made generic
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
    ("Generalizing region"
     (curr_region
      ((region ((status Alive) (types ())))
       (node
        ((id 4)
         (parent
          (((id 1)
            (parent
             (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
            (level 1)
            (region
             ((status Alive)
              (types
               ((Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 3) (guards ()) (inner (Var Empty))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 2) (guards ()) (inner (Var Empty))
                      (status (Instance <opaque>)))))))))))))))
         (level 2) (region ((status Alive) (types ())))))
       (path
        ((dst
          ((id 4)
           (parent
            (((id 1)
              (parent
               (((id 0) (parent ()) (level 0)
                 (region ((status Alive) (types ()))))))
              (level 1)
              (region
               ((status Alive)
                (types
                 ((Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 3) (guards ()) (inner (Var Empty))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 2) (guards ()) (inner (Var Empty))
                        (status (Instance <opaque>)))))))))))))))
           (level 2) (region ((status Alive) (types ())))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    ("Suspend on"
     (suspended_match.matchee
      (Root
       ((rank 0)
        (value
         ((structure
           ((id 2) (guards ())
            (inner (Var (Empty_one_or_more_handlers (((run <fun>))))))
            (status (Instance <opaque>)))))))))
     (guard 5))
    svar filled and handlers fired
    ("Removing guard"
     (t
      (Root
       ((rank 0)
        (value
         ((structure
           ((id 3) (guards (5)) (inner (Var Empty)) (status (Instance <opaque>)))))))))
     (guard 5))
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
    ("Suspend on"
     (suspended_match.matchee
      (Root
       ((rank 0)
        (value
         ((structure
           ((id 2) (guards ())
            (inner (Var (Empty_one_or_more_handlers (((run <fun>))))))
            (status (Instance <opaque>)))))))))
     (guard 5))
    ("Generalizing region"
     (curr_region
      ((region
        ((status Alive)
         (types
          ((Root
            ((rank 0)
             (value
              ((structure
                ((id 4) (guards (5)) (inner (Var Empty))
                 (status (Instance <opaque>))))))))))))
       (node
        ((id 3)
         (parent
          (((id 1)
            (parent
             (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
            (level 1)
            (region
             ((status Alive)
              (types
               ((Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 2) (guards ())
                      (inner
                       (Var
                        (Empty_one_or_more_handlers
                         (((run <fun>)) ((run <fun>))))))
                      (status (Instance <opaque>)))))))))))))))
         (level 2)
         (region
          ((status Alive)
           (types
            ((Root
              ((rank 0)
               (value
                ((structure
                  ((id 4) (guards (5)) (inner (Var Empty))
                   (status (Instance <opaque>))))))))))))))
       (path
        ((dst
          ((id 3)
           (parent
            (((id 1)
              (parent
               (((id 0) (parent ()) (level 0)
                 (region ((status Alive) (types ()))))))
              (level 1)
              (region
               ((status Alive)
                (types
                 ((Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 2) (guards ())
                        (inner
                         (Var
                          (Empty_one_or_more_handlers
                           (((run <fun>)) ((run <fun>))))))
                        (status (Instance <opaque>)))))))))))))))
           (level 2)
           (region
            ((status Alive)
             (types
              ((Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 4) (guards (5)) (inner (Var Empty))
                     (status (Instance <opaque>))))))))))))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    Type is made partial generic
    (id (id 5))
    Generaizing alive region into zombie region
    ("Zombie region"
     (partial_generics
      ((Root
        ((rank 0)
         (value
          ((structure
            ((id 4) (guards (5)) (inner (Var Empty))
             (status (Partial_generic (region_node <opaque>) (instances ()))))))))))))
    svar filled and handlers fired
    ("Removing guard"
     (t
      (Root
       ((rank 0)
        (value
         ((structure
           ((id 4) (guards (5)) (inner (Var Empty))
            (status
             (Partial_generic (region_node <opaque>)
              (instances
               ((4
                 (Inner
                  (Root
                   ((rank 1)
                    (value
                     ((structure
                       ((id 13) (guards (4))
                        (inner
                         (Structure
                          (Arrow
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 11) (guards ())
                                 (inner (Structure (Constr () string)))
                                 (status (Instance <opaque>))))))))
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 10) (guards ())
                                 (inner (Structure (Constr () string)))
                                 (status (Instance <opaque>)))))))))))
                        (status (Instance <opaque>))))))))))
                (4
                 (Inner
                  (Root
                   ((rank 1)
                    (value
                     ((structure
                       ((id 9) (guards (4))
                        (inner
                         (Structure
                          (Arrow
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 7) (guards ())
                                 (inner (Structure (Constr () int)))
                                 (status (Instance <opaque>))))))))
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 6) (guards ())
                                 (inner (Structure (Constr () int)))
                                 (status (Instance <opaque>)))))))))))
                        (status (Instance <opaque>)))))))))))))))))))))
     (guard 5))
    ("Generalizing region"
     (curr_region
      ((region
        ((status Zombie)
         (types
          ((Root
            ((rank 0)
             (value
              ((structure
                ((id 4) (guards ()) (inner (Var Empty))
                 (status
                  (Partial_generic (region_node <opaque>)
                   (instances
                    ((4
                      (Inner
                       (Root
                        ((rank 1)
                         (value
                          ((structure
                            ((id 13) (guards (4))
                             (inner
                              (Structure
                               (Arrow
                                (Root
                                 ((rank 0)
                                  (value
                                   ((structure
                                     ((id 11) (guards ())
                                      (inner (Structure (Constr () string)))
                                      (status (Instance <opaque>))))))))
                                (Root
                                 ((rank 0)
                                  (value
                                   ((structure
                                     ((id 10) (guards ())
                                      (inner (Structure (Constr () string)))
                                      (status (Instance <opaque>)))))))))))
                             (status (Instance <opaque>))))))))))
                     (4
                      (Inner
                       (Root
                        ((rank 1)
                         (value
                          ((structure
                            ((id 9) (guards (4))
                             (inner
                              (Structure
                               (Arrow
                                (Root
                                 ((rank 0)
                                  (value
                                   ((structure
                                     ((id 7) (guards ())
                                      (inner (Structure (Constr () int)))
                                      (status (Instance <opaque>))))))))
                                (Root
                                 ((rank 0)
                                  (value
                                   ((structure
                                     ((id 6) (guards ())
                                      (inner (Structure (Constr () int)))
                                      (status (Instance <opaque>)))))))))))
                             (status (Instance <opaque>))))))))))))))))))))))))
       (node
        ((id 3)
         (parent
          (((id 1)
            (parent
             (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
            (level 1)
            (region
             ((status Alive)
              (types
               ((Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 2) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>))))))))
                (Inner
                 (Root
                  ((rank 1)
                   (value
                    ((structure
                      ((id 13) (guards (4))
                       (inner
                        (Structure
                         (Arrow
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 11) (guards ())
                                (inner (Structure (Constr () string)))
                                (status (Instance <opaque>))))))))
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 10) (guards ())
                                (inner (Structure (Constr () string)))
                                (status (Instance <opaque>)))))))))))
                       (status (Instance <opaque>)))))))))
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 13) (guards (4))
                      (inner
                       (Structure
                        (Arrow
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 11) (guards ())
                               (inner (Structure (Constr () string)))
                               (status (Instance <opaque>))))))))
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 10) (guards ())
                               (inner (Structure (Constr () string)))
                               (status (Instance <opaque>)))))))))))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 11) (guards ()) (inner (Structure (Constr () string)))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 10) (guards ()) (inner (Structure (Constr () string)))
                      (status (Instance <opaque>))))))))
                (Inner
                 (Root
                  ((rank 1)
                   (value
                    ((structure
                      ((id 9) (guards (4))
                       (inner
                        (Structure
                         (Arrow
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 7) (guards ())
                                (inner (Structure (Constr () int)))
                                (status (Instance <opaque>))))))))
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 6) (guards ())
                                (inner (Structure (Constr () int)))
                                (status (Instance <opaque>)))))))))))
                       (status (Instance <opaque>)))))))))
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 9) (guards (4))
                      (inner
                       (Structure
                        (Arrow
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 7) (guards ())
                               (inner (Structure (Constr () int)))
                               (status (Instance <opaque>))))))))
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 6) (guards ())
                               (inner (Structure (Constr () int)))
                               (status (Instance <opaque>)))))))))))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 7) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 6) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>))))))))
                (Inner
                 (Root
                  ((rank 1)
                   (value
                    ((structure
                      ((id 2) (guards ()) (inner (Structure (Constr () int)))
                       (status (Instance <opaque>))))))))))))))))
         (level 2)
         (region
          ((status Zombie)
           (types
            ((Root
              ((rank 0)
               (value
                ((structure
                  ((id 4) (guards ()) (inner (Var Empty))
                   (status
                    (Partial_generic (region_node <opaque>)
                     (instances
                      ((4
                        (Inner
                         (Root
                          ((rank 1)
                           (value
                            ((structure
                              ((id 13) (guards (4))
                               (inner
                                (Structure
                                 (Arrow
                                  (Root
                                   ((rank 0)
                                    (value
                                     ((structure
                                       ((id 11) (guards ())
                                        (inner (Structure (Constr () string)))
                                        (status (Instance <opaque>))))))))
                                  (Root
                                   ((rank 0)
                                    (value
                                     ((structure
                                       ((id 10) (guards ())
                                        (inner (Structure (Constr () string)))
                                        (status (Instance <opaque>)))))))))))
                               (status (Instance <opaque>))))))))))
                       (4
                        (Inner
                         (Root
                          ((rank 1)
                           (value
                            ((structure
                              ((id 9) (guards (4))
                               (inner
                                (Structure
                                 (Arrow
                                  (Root
                                   ((rank 0)
                                    (value
                                     ((structure
                                       ((id 7) (guards ())
                                        (inner (Structure (Constr () int)))
                                        (status (Instance <opaque>))))))))
                                  (Root
                                   ((rank 0)
                                    (value
                                     ((structure
                                       ((id 6) (guards ())
                                        (inner (Structure (Constr () int)))
                                        (status (Instance <opaque>)))))))))))
                               (status (Instance <opaque>))))))))))))))))))))))))))
       (path
        ((dst
          ((id 3)
           (parent
            (((id 1)
              (parent
               (((id 0) (parent ()) (level 0)
                 (region ((status Alive) (types ()))))))
              (level 1)
              (region
               ((status Alive)
                (types
                 ((Root
                   ((rank 1)
                    (value
                     ((structure
                       ((id 2) (guards ()) (inner (Structure (Constr () int)))
                        (status (Instance <opaque>))))))))
                  (Inner
                   (Root
                    ((rank 1)
                     (value
                      ((structure
                        ((id 13) (guards (4))
                         (inner
                          (Structure
                           (Arrow
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 11) (guards ())
                                  (inner (Structure (Constr () string)))
                                  (status (Instance <opaque>))))))))
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 10) (guards ())
                                  (inner (Structure (Constr () string)))
                                  (status (Instance <opaque>)))))))))))
                         (status (Instance <opaque>)))))))))
                  (Root
                   ((rank 1)
                    (value
                     ((structure
                       ((id 13) (guards (4))
                        (inner
                         (Structure
                          (Arrow
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 11) (guards ())
                                 (inner (Structure (Constr () string)))
                                 (status (Instance <opaque>))))))))
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 10) (guards ())
                                 (inner (Structure (Constr () string)))
                                 (status (Instance <opaque>)))))))))))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 11) (guards ())
                        (inner (Structure (Constr () string)))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 10) (guards ())
                        (inner (Structure (Constr () string)))
                        (status (Instance <opaque>))))))))
                  (Inner
                   (Root
                    ((rank 1)
                     (value
                      ((structure
                        ((id 9) (guards (4))
                         (inner
                          (Structure
                           (Arrow
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 7) (guards ())
                                  (inner (Structure (Constr () int)))
                                  (status (Instance <opaque>))))))))
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 6) (guards ())
                                  (inner (Structure (Constr () int)))
                                  (status (Instance <opaque>)))))))))))
                         (status (Instance <opaque>)))))))))
                  (Root
                   ((rank 1)
                    (value
                     ((structure
                       ((id 9) (guards (4))
                        (inner
                         (Structure
                          (Arrow
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 7) (guards ())
                                 (inner (Structure (Constr () int)))
                                 (status (Instance <opaque>))))))))
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 6) (guards ())
                                 (inner (Structure (Constr () int)))
                                 (status (Instance <opaque>)))))))))))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 7) (guards ()) (inner (Structure (Constr () int)))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 6) (guards ()) (inner (Structure (Constr () int)))
                        (status (Instance <opaque>))))))))
                  (Inner
                   (Root
                    ((rank 1)
                     (value
                      ((structure
                        ((id 2) (guards ()) (inner (Structure (Constr () int)))
                         (status (Instance <opaque>))))))))))))))))
           (level 2)
           (region
            ((status Zombie)
             (types
              ((Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 4) (guards ()) (inner (Var Empty))
                     (status
                      (Partial_generic (region_node <opaque>)
                       (instances
                        ((4
                          (Inner
                           (Root
                            ((rank 1)
                             (value
                              ((structure
                                ((id 13) (guards (4))
                                 (inner
                                  (Structure
                                   (Arrow
                                    (Root
                                     ((rank 0)
                                      (value
                                       ((structure
                                         ((id 11) (guards ())
                                          (inner (Structure (Constr () string)))
                                          (status (Instance <opaque>))))))))
                                    (Root
                                     ((rank 0)
                                      (value
                                       ((structure
                                         ((id 10) (guards ())
                                          (inner (Structure (Constr () string)))
                                          (status (Instance <opaque>)))))))))))
                                 (status (Instance <opaque>))))))))))
                         (4
                          (Inner
                           (Root
                            ((rank 1)
                             (value
                              ((structure
                                ((id 9) (guards (4))
                                 (inner
                                  (Structure
                                   (Arrow
                                    (Root
                                     ((rank 0)
                                      (value
                                       ((structure
                                         ((id 7) (guards ())
                                          (inner (Structure (Constr () int)))
                                          (status (Instance <opaque>))))))))
                                    (Root
                                     ((rank 0)
                                      (value
                                       ((structure
                                         ((id 6) (guards ())
                                          (inner (Structure (Constr () int)))
                                          (status (Instance <opaque>)))))))))))
                                 (status (Instance <opaque>))))))))))))))))))))))))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    ("Generalizing region"
     (curr_region
      ((region
        ((status Zombie)
         (types
          ((Root
            ((rank 1)
             (value
              ((structure
                ((id 4) (guards ())
                 (inner
                  (Structure
                   (Arrow
                    (Root
                     ((rank 0)
                      (value
                       ((structure
                         ((id 16) (guards ()) (inner (Var Empty))
                          (status
                           (Partial_instance (region_node <opaque>)
                            (instances ())))))))))
                    (Root
                     ((rank 0)
                      (value
                       ((structure
                         ((id 16) (guards ()) (inner (Var Empty))
                          (status
                           (Partial_instance (region_node <opaque>)
                            (instances ()))))))))))))
                 (status
                  (Partial_instance (region_node <opaque>)
                   (instances
                    ((4
                      (Inner
                       (Root
                        ((rank 0)
                         (value
                          ((structure
                            ((id 15) (guards (4))
                             (inner
                              (Structure
                               (Arrow
                                (Root
                                 ((rank 0)
                                  (value
                                   ((structure
                                     ((id 11) (guards ())
                                      (inner (Structure (Constr () string)))
                                      (status (Instance <opaque>))))))))
                                (Root
                                 ((rank 0)
                                  (value
                                   ((structure
                                     ((id 10) (guards ())
                                      (inner (Structure (Constr () string)))
                                      (status (Instance <opaque>)))))))))))
                             (status (Instance <opaque>))))))))))
                     (4
                      (Inner
                       (Root
                        ((rank 1)
                         (value
                          ((structure
                            ((id 9) (guards (4))
                             (inner
                              (Structure
                               (Arrow
                                (Root
                                 ((rank 0)
                                  (value
                                   ((structure
                                     ((id 7) (guards ())
                                      (inner (Structure (Constr () int)))
                                      (status (Instance <opaque>))))))))
                                (Root
                                 ((rank 0)
                                  (value
                                   ((structure
                                     ((id 6) (guards ())
                                      (inner (Structure (Constr () int)))
                                      (status (Instance <opaque>)))))))))))
                             (status (Instance <opaque>))))))))))))))))))))
           (Root
            ((rank 0)
             (value
              ((structure
                ((id 16) (guards ()) (inner (Var Empty))
                 (status
                  (Partial_instance (region_node <opaque>) (instances ())))))))))
           (Inner
            (Root
             ((rank 1)
              (value
               ((structure
                 ((id 4) (guards ())
                  (inner
                   (Structure
                    (Arrow
                     (Root
                      ((rank 0)
                       (value
                        ((structure
                          ((id 16) (guards ()) (inner (Var Empty))
                           (status
                            (Partial_instance (region_node <opaque>)
                             (instances ())))))))))
                     (Root
                      ((rank 0)
                       (value
                        ((structure
                          ((id 16) (guards ()) (inner (Var Empty))
                           (status
                            (Partial_instance (region_node <opaque>)
                             (instances ()))))))))))))
                  (status
                   (Partial_instance (region_node <opaque>)
                    (instances
                     ((4
                       (Inner
                        (Root
                         ((rank 0)
                          (value
                           ((structure
                             ((id 15) (guards (4))
                              (inner
                               (Structure
                                (Arrow
                                 (Root
                                  ((rank 0)
                                   (value
                                    ((structure
                                      ((id 11) (guards ())
                                       (inner (Structure (Constr () string)))
                                       (status (Instance <opaque>))))))))
                                 (Root
                                  ((rank 0)
                                   (value
                                    ((structure
                                      ((id 10) (guards ())
                                       (inner (Structure (Constr () string)))
                                       (status (Instance <opaque>)))))))))))
                              (status (Instance <opaque>))))))))))
                      (4
                       (Inner
                        (Root
                         ((rank 1)
                          (value
                           ((structure
                             ((id 9) (guards (4))
                              (inner
                               (Structure
                                (Arrow
                                 (Root
                                  ((rank 0)
                                   (value
                                    ((structure
                                      ((id 7) (guards ())
                                       (inner (Structure (Constr () int)))
                                       (status (Instance <opaque>))))))))
                                 (Root
                                  ((rank 0)
                                   (value
                                    ((structure
                                      ((id 6) (guards ())
                                       (inner (Structure (Constr () int)))
                                       (status (Instance <opaque>)))))))))))
                              (status (Instance <opaque>)))))))))))))))))))))))))
       (node
        ((id 3)
         (parent
          (((id 1)
            (parent
             (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
            (level 1)
            (region
             ((status Alive)
              (types
               ((Inner
                 (Root
                  ((rank 0)
                   (value
                    ((structure
                      ((id 15) (guards (4))
                       (inner
                        (Structure
                         (Arrow
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 11) (guards ())
                                (inner (Structure (Constr () string)))
                                (status (Instance <opaque>))))))))
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 10) (guards ())
                                (inner (Structure (Constr () string)))
                                (status (Instance <opaque>)))))))))))
                       (status (Instance <opaque>)))))))))
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 2) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>))))))))
                (Inner
                 (Root
                  ((rank 0)
                   (value
                    ((structure
                      ((id 15) (guards (4))
                       (inner
                        (Structure
                         (Arrow
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 11) (guards ())
                                (inner (Structure (Constr () string)))
                                (status (Instance <opaque>))))))))
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 10) (guards ())
                                (inner (Structure (Constr () string)))
                                (status (Instance <opaque>)))))))))))
                       (status (Instance <opaque>)))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 15) (guards (4))
                      (inner
                       (Structure
                        (Arrow
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 11) (guards ())
                               (inner (Structure (Constr () string)))
                               (status (Instance <opaque>))))))))
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 10) (guards ())
                               (inner (Structure (Constr () string)))
                               (status (Instance <opaque>)))))))))))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 11) (guards ()) (inner (Structure (Constr () string)))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 10) (guards ()) (inner (Structure (Constr () string)))
                      (status (Instance <opaque>))))))))
                (Inner
                 (Root
                  ((rank 1)
                   (value
                    ((structure
                      ((id 9) (guards (4))
                       (inner
                        (Structure
                         (Arrow
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 7) (guards ())
                                (inner (Structure (Constr () int)))
                                (status (Instance <opaque>))))))))
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 6) (guards ())
                                (inner (Structure (Constr () int)))
                                (status (Instance <opaque>)))))))))))
                       (status (Instance <opaque>)))))))))
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 9) (guards (4))
                      (inner
                       (Structure
                        (Arrow
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 7) (guards ())
                               (inner (Structure (Constr () int)))
                               (status (Instance <opaque>))))))))
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 6) (guards ())
                               (inner (Structure (Constr () int)))
                               (status (Instance <opaque>)))))))))))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 7) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 6) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>))))))))
                (Inner
                 (Root
                  ((rank 1)
                   (value
                    ((structure
                      ((id 2) (guards ()) (inner (Structure (Constr () int)))
                       (status (Instance <opaque>))))))))))))))))
         (level 2)
         (region
          ((status Zombie)
           (types
            ((Root
              ((rank 1)
               (value
                ((structure
                  ((id 4) (guards ())
                   (inner
                    (Structure
                     (Arrow
                      (Root
                       ((rank 0)
                        (value
                         ((structure
                           ((id 16) (guards ()) (inner (Var Empty))
                            (status
                             (Partial_instance (region_node <opaque>)
                              (instances ())))))))))
                      (Root
                       ((rank 0)
                        (value
                         ((structure
                           ((id 16) (guards ()) (inner (Var Empty))
                            (status
                             (Partial_instance (region_node <opaque>)
                              (instances ()))))))))))))
                   (status
                    (Partial_instance (region_node <opaque>)
                     (instances
                      ((4
                        (Inner
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 15) (guards (4))
                               (inner
                                (Structure
                                 (Arrow
                                  (Root
                                   ((rank 0)
                                    (value
                                     ((structure
                                       ((id 11) (guards ())
                                        (inner (Structure (Constr () string)))
                                        (status (Instance <opaque>))))))))
                                  (Root
                                   ((rank 0)
                                    (value
                                     ((structure
                                       ((id 10) (guards ())
                                        (inner (Structure (Constr () string)))
                                        (status (Instance <opaque>)))))))))))
                               (status (Instance <opaque>))))))))))
                       (4
                        (Inner
                         (Root
                          ((rank 1)
                           (value
                            ((structure
                              ((id 9) (guards (4))
                               (inner
                                (Structure
                                 (Arrow
                                  (Root
                                   ((rank 0)
                                    (value
                                     ((structure
                                       ((id 7) (guards ())
                                        (inner (Structure (Constr () int)))
                                        (status (Instance <opaque>))))))))
                                  (Root
                                   ((rank 0)
                                    (value
                                     ((structure
                                       ((id 6) (guards ())
                                        (inner (Structure (Constr () int)))
                                        (status (Instance <opaque>)))))))))))
                               (status (Instance <opaque>))))))))))))))))))))
             (Root
              ((rank 0)
               (value
                ((structure
                  ((id 16) (guards ()) (inner (Var Empty))
                   (status
                    (Partial_instance (region_node <opaque>) (instances ())))))))))
             (Inner
              (Root
               ((rank 1)
                (value
                 ((structure
                   ((id 4) (guards ())
                    (inner
                     (Structure
                      (Arrow
                       (Root
                        ((rank 0)
                         (value
                          ((structure
                            ((id 16) (guards ()) (inner (Var Empty))
                             (status
                              (Partial_instance (region_node <opaque>)
                               (instances ())))))))))
                       (Root
                        ((rank 0)
                         (value
                          ((structure
                            ((id 16) (guards ()) (inner (Var Empty))
                             (status
                              (Partial_instance (region_node <opaque>)
                               (instances ()))))))))))))
                    (status
                     (Partial_instance (region_node <opaque>)
                      (instances
                       ((4
                         (Inner
                          (Root
                           ((rank 0)
                            (value
                             ((structure
                               ((id 15) (guards (4))
                                (inner
                                 (Structure
                                  (Arrow
                                   (Root
                                    ((rank 0)
                                     (value
                                      ((structure
                                        ((id 11) (guards ())
                                         (inner (Structure (Constr () string)))
                                         (status (Instance <opaque>))))))))
                                   (Root
                                    ((rank 0)
                                     (value
                                      ((structure
                                        ((id 10) (guards ())
                                         (inner (Structure (Constr () string)))
                                         (status (Instance <opaque>)))))))))))
                                (status (Instance <opaque>))))))))))
                        (4
                         (Inner
                          (Root
                           ((rank 1)
                            (value
                             ((structure
                               ((id 9) (guards (4))
                                (inner
                                 (Structure
                                  (Arrow
                                   (Root
                                    ((rank 0)
                                     (value
                                      ((structure
                                        ((id 7) (guards ())
                                         (inner (Structure (Constr () int)))
                                         (status (Instance <opaque>))))))))
                                   (Root
                                    ((rank 0)
                                     (value
                                      ((structure
                                        ((id 6) (guards ())
                                         (inner (Structure (Constr () int)))
                                         (status (Instance <opaque>)))))))))))
                                (status (Instance <opaque>)))))))))))))))))))))))))))
       (path
        ((dst
          ((id 3)
           (parent
            (((id 1)
              (parent
               (((id 0) (parent ()) (level 0)
                 (region ((status Alive) (types ()))))))
              (level 1)
              (region
               ((status Alive)
                (types
                 ((Inner
                   (Root
                    ((rank 0)
                     (value
                      ((structure
                        ((id 15) (guards (4))
                         (inner
                          (Structure
                           (Arrow
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 11) (guards ())
                                  (inner (Structure (Constr () string)))
                                  (status (Instance <opaque>))))))))
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 10) (guards ())
                                  (inner (Structure (Constr () string)))
                                  (status (Instance <opaque>)))))))))))
                         (status (Instance <opaque>)))))))))
                  (Root
                   ((rank 1)
                    (value
                     ((structure
                       ((id 2) (guards ()) (inner (Structure (Constr () int)))
                        (status (Instance <opaque>))))))))
                  (Inner
                   (Root
                    ((rank 0)
                     (value
                      ((structure
                        ((id 15) (guards (4))
                         (inner
                          (Structure
                           (Arrow
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 11) (guards ())
                                  (inner (Structure (Constr () string)))
                                  (status (Instance <opaque>))))))))
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 10) (guards ())
                                  (inner (Structure (Constr () string)))
                                  (status (Instance <opaque>)))))))))))
                         (status (Instance <opaque>)))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 15) (guards (4))
                        (inner
                         (Structure
                          (Arrow
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 11) (guards ())
                                 (inner (Structure (Constr () string)))
                                 (status (Instance <opaque>))))))))
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 10) (guards ())
                                 (inner (Structure (Constr () string)))
                                 (status (Instance <opaque>)))))))))))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 11) (guards ())
                        (inner (Structure (Constr () string)))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 10) (guards ())
                        (inner (Structure (Constr () string)))
                        (status (Instance <opaque>))))))))
                  (Inner
                   (Root
                    ((rank 1)
                     (value
                      ((structure
                        ((id 9) (guards (4))
                         (inner
                          (Structure
                           (Arrow
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 7) (guards ())
                                  (inner (Structure (Constr () int)))
                                  (status (Instance <opaque>))))))))
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 6) (guards ())
                                  (inner (Structure (Constr () int)))
                                  (status (Instance <opaque>)))))))))))
                         (status (Instance <opaque>)))))))))
                  (Root
                   ((rank 1)
                    (value
                     ((structure
                       ((id 9) (guards (4))
                        (inner
                         (Structure
                          (Arrow
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 7) (guards ())
                                 (inner (Structure (Constr () int)))
                                 (status (Instance <opaque>))))))))
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 6) (guards ())
                                 (inner (Structure (Constr () int)))
                                 (status (Instance <opaque>)))))))))))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 7) (guards ()) (inner (Structure (Constr () int)))
                        (status (Instance <opaque>))))))))
                  (Root
                   ((rank 0)
                    (value
                     ((structure
                       ((id 6) (guards ()) (inner (Structure (Constr () int)))
                        (status (Instance <opaque>))))))))
                  (Inner
                   (Root
                    ((rank 1)
                     (value
                      ((structure
                        ((id 2) (guards ()) (inner (Structure (Constr () int)))
                         (status (Instance <opaque>))))))))))))))))
           (level 2)
           (region
            ((status Zombie)
             (types
              ((Root
                ((rank 1)
                 (value
                  ((structure
                    ((id 4) (guards ())
                     (inner
                      (Structure
                       (Arrow
                        (Root
                         ((rank 0)
                          (value
                           ((structure
                             ((id 16) (guards ()) (inner (Var Empty))
                              (status
                               (Partial_instance (region_node <opaque>)
                                (instances ())))))))))
                        (Root
                         ((rank 0)
                          (value
                           ((structure
                             ((id 16) (guards ()) (inner (Var Empty))
                              (status
                               (Partial_instance (region_node <opaque>)
                                (instances ()))))))))))))
                     (status
                      (Partial_instance (region_node <opaque>)
                       (instances
                        ((4
                          (Inner
                           (Root
                            ((rank 0)
                             (value
                              ((structure
                                ((id 15) (guards (4))
                                 (inner
                                  (Structure
                                   (Arrow
                                    (Root
                                     ((rank 0)
                                      (value
                                       ((structure
                                         ((id 11) (guards ())
                                          (inner (Structure (Constr () string)))
                                          (status (Instance <opaque>))))))))
                                    (Root
                                     ((rank 0)
                                      (value
                                       ((structure
                                         ((id 10) (guards ())
                                          (inner (Structure (Constr () string)))
                                          (status (Instance <opaque>)))))))))))
                                 (status (Instance <opaque>))))))))))
                         (4
                          (Inner
                           (Root
                            ((rank 1)
                             (value
                              ((structure
                                ((id 9) (guards (4))
                                 (inner
                                  (Structure
                                   (Arrow
                                    (Root
                                     ((rank 0)
                                      (value
                                       ((structure
                                         ((id 7) (guards ())
                                          (inner (Structure (Constr () int)))
                                          (status (Instance <opaque>))))))))
                                    (Root
                                     ((rank 0)
                                      (value
                                       ((structure
                                         ((id 6) (guards ())
                                          (inner (Structure (Constr () int)))
                                          (status (Instance <opaque>)))))))))))
                                 (status (Instance <opaque>))))))))))))))))))))
               (Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 16) (guards ()) (inner (Var Empty))
                     (status
                      (Partial_instance (region_node <opaque>) (instances ())))))))))
               (Inner
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 4) (guards ())
                      (inner
                       (Structure
                        (Arrow
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 16) (guards ()) (inner (Var Empty))
                               (status
                                (Partial_instance (region_node <opaque>)
                                 (instances ())))))))))
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 16) (guards ()) (inner (Var Empty))
                               (status
                                (Partial_instance (region_node <opaque>)
                                 (instances ()))))))))))))
                      (status
                       (Partial_instance (region_node <opaque>)
                        (instances
                         ((4
                           (Inner
                            (Root
                             ((rank 0)
                              (value
                               ((structure
                                 ((id 15) (guards (4))
                                  (inner
                                   (Structure
                                    (Arrow
                                     (Root
                                      ((rank 0)
                                       (value
                                        ((structure
                                          ((id 11) (guards ())
                                           (inner (Structure (Constr () string)))
                                           (status (Instance <opaque>))))))))
                                     (Root
                                      ((rank 0)
                                       (value
                                        ((structure
                                          ((id 10) (guards ())
                                           (inner (Structure (Constr () string)))
                                           (status (Instance <opaque>)))))))))))
                                  (status (Instance <opaque>))))))))))
                          (4
                           (Inner
                            (Root
                             ((rank 1)
                              (value
                               ((structure
                                 ((id 9) (guards (4))
                                  (inner
                                   (Structure
                                    (Arrow
                                     (Root
                                      ((rank 0)
                                       (value
                                        ((structure
                                          ((id 7) (guards ())
                                           (inner (Structure (Constr () int)))
                                           (status (Instance <opaque>))))))))
                                     (Root
                                      ((rank 0)
                                       (value
                                        ((structure
                                          ((id 6) (guards ())
                                           (inner (Structure (Constr () int)))
                                           (status (Instance <opaque>)))))))))))
                                  (status (Instance <opaque>)))))))))))))))))))))))))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    Generalizing zombie region into dead region
    ("Removing guard"
     (t
      (Inner
       (Root
        ((rank 0)
         (value
          ((structure
            ((id 15) (guards (4))
             (inner
              (Structure
               (Arrow
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 11) (guards ()) (inner (Structure (Constr () string)))
                      (status (Instance <opaque>))))))))
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 10) (guards ()) (inner (Structure (Constr () string)))
                      (status (Instance <opaque>)))))))))))
             (status (Instance <opaque>))))))))))
     (guard 4))
    ("Removing guard"
     (t
      (Inner
       (Root
        ((rank 0)
         (value
          ((structure
            ((id 18) (guards (4))
             (inner
              (Structure
               (Arrow
                (Inner
                 (Root
                  ((rank 1)
                   (value
                    ((structure
                      ((id 19) (guards ()) (inner (Structure (Constr () int)))
                       (status (Instance <opaque>)))))))))
                (Inner
                 (Root
                  ((rank 1)
                   (value
                    ((structure
                      ((id 19) (guards ()) (inner (Structure (Constr () int)))
                       (status (Instance <opaque>))))))))))))
             (status (Instance <opaque>))))))))))
     (guard 4))
    Generalizing zombie region into dead region
    ("Generalizing region"
     (curr_region
      ((region
        ((status Alive)
         (types
          ((Root
            ((rank 1)
             (value
              ((structure
                ((id 19) (guards ()) (inner (Structure (Constr () int)))
                 (status (Instance <opaque>))))))))
           (Root
            ((rank 0)
             (value
              ((structure
                ((id 18) (guards ())
                 (inner
                  (Structure
                   (Arrow
                    (Root
                     ((rank 1)
                      (value
                       ((structure
                         ((id 19) (guards ()) (inner (Structure (Constr () int)))
                          (status (Instance <opaque>))))))))
                    (Root
                     ((rank 1)
                      (value
                       ((structure
                         ((id 19) (guards ()) (inner (Structure (Constr () int)))
                          (status (Instance <opaque>)))))))))))
                 (status (Instance <opaque>))))))))
           (Root
            ((rank 0)
             (value
              ((structure
                ((id 15) (guards ())
                 (inner
                  (Structure
                   (Arrow
                    (Root
                     ((rank 0)
                      (value
                       ((structure
                         ((id 11) (guards ())
                          (inner (Structure (Constr () string)))
                          (status (Instance <opaque>))))))))
                    (Root
                     ((rank 0)
                      (value
                       ((structure
                         ((id 10) (guards ())
                          (inner (Structure (Constr () string)))
                          (status (Instance <opaque>)))))))))))
                 (status (Instance <opaque>))))))))
           (Root
            ((rank 1)
             (value
              ((structure
                ((id 2) (guards ()) (inner (Structure (Constr () int)))
                 (status (Instance <opaque>))))))))
           (Inner
            (Root
             ((rank 0)
              (value
               ((structure
                 ((id 15) (guards ())
                  (inner
                   (Structure
                    (Arrow
                     (Root
                      ((rank 0)
                       (value
                        ((structure
                          ((id 11) (guards ())
                           (inner (Structure (Constr () string)))
                           (status (Instance <opaque>))))))))
                     (Root
                      ((rank 0)
                       (value
                        ((structure
                          ((id 10) (guards ())
                           (inner (Structure (Constr () string)))
                           (status (Instance <opaque>)))))))))))
                  (status (Instance <opaque>)))))))))
           (Root
            ((rank 0)
             (value
              ((structure
                ((id 15) (guards ())
                 (inner
                  (Structure
                   (Arrow
                    (Root
                     ((rank 0)
                      (value
                       ((structure
                         ((id 11) (guards ())
                          (inner (Structure (Constr () string)))
                          (status (Instance <opaque>))))))))
                    (Root
                     ((rank 0)
                      (value
                       ((structure
                         ((id 10) (guards ())
                          (inner (Structure (Constr () string)))
                          (status (Instance <opaque>)))))))))))
                 (status (Instance <opaque>))))))))
           (Root
            ((rank 0)
             (value
              ((structure
                ((id 11) (guards ()) (inner (Structure (Constr () string)))
                 (status (Instance <opaque>))))))))
           (Root
            ((rank 0)
             (value
              ((structure
                ((id 10) (guards ()) (inner (Structure (Constr () string)))
                 (status (Instance <opaque>))))))))
           (Inner
            (Root
             ((rank 0)
              (value
               ((structure
                 ((id 18) (guards ())
                  (inner
                   (Structure
                    (Arrow
                     (Root
                      ((rank 1)
                       (value
                        ((structure
                          ((id 19) (guards ())
                           (inner (Structure (Constr () int)))
                           (status (Instance <opaque>))))))))
                     (Root
                      ((rank 1)
                       (value
                        ((structure
                          ((id 19) (guards ())
                           (inner (Structure (Constr () int)))
                           (status (Instance <opaque>)))))))))))
                  (status (Instance <opaque>)))))))))
           (Root
            ((rank 0)
             (value
              ((structure
                ((id 18) (guards ())
                 (inner
                  (Structure
                   (Arrow
                    (Root
                     ((rank 1)
                      (value
                       ((structure
                         ((id 19) (guards ()) (inner (Structure (Constr () int)))
                          (status (Instance <opaque>))))))))
                    (Root
                     ((rank 1)
                      (value
                       ((structure
                         ((id 19) (guards ()) (inner (Structure (Constr () int)))
                          (status (Instance <opaque>)))))))))))
                 (status (Instance <opaque>))))))))
           (Root
            ((rank 1)
             (value
              ((structure
                ((id 19) (guards ()) (inner (Structure (Constr () int)))
                 (status (Instance <opaque>))))))))
           (Inner
            (Root
             ((rank 1)
              (value
               ((structure
                 ((id 19) (guards ()) (inner (Structure (Constr () int)))
                  (status (Instance <opaque>)))))))))
           (Inner
            (Root
             ((rank 1)
              (value
               ((structure
                 ((id 2) (guards ()) (inner (Structure (Constr () int)))
                  (status (Instance <opaque>)))))))))))))
       (node
        ((id 1)
         (parent
          (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
         (level 1)
         (region
          ((status Alive)
           (types
            ((Root
              ((rank 1)
               (value
                ((structure
                  ((id 19) (guards ()) (inner (Structure (Constr () int)))
                   (status (Instance <opaque>))))))))
             (Root
              ((rank 0)
               (value
                ((structure
                  ((id 18) (guards ())
                   (inner
                    (Structure
                     (Arrow
                      (Root
                       ((rank 1)
                        (value
                         ((structure
                           ((id 19) (guards ())
                            (inner (Structure (Constr () int)))
                            (status (Instance <opaque>))))))))
                      (Root
                       ((rank 1)
                        (value
                         ((structure
                           ((id 19) (guards ())
                            (inner (Structure (Constr () int)))
                            (status (Instance <opaque>)))))))))))
                   (status (Instance <opaque>))))))))
             (Root
              ((rank 0)
               (value
                ((structure
                  ((id 15) (guards ())
                   (inner
                    (Structure
                     (Arrow
                      (Root
                       ((rank 0)
                        (value
                         ((structure
                           ((id 11) (guards ())
                            (inner (Structure (Constr () string)))
                            (status (Instance <opaque>))))))))
                      (Root
                       ((rank 0)
                        (value
                         ((structure
                           ((id 10) (guards ())
                            (inner (Structure (Constr () string)))
                            (status (Instance <opaque>)))))))))))
                   (status (Instance <opaque>))))))))
             (Root
              ((rank 1)
               (value
                ((structure
                  ((id 2) (guards ()) (inner (Structure (Constr () int)))
                   (status (Instance <opaque>))))))))
             (Inner
              (Root
               ((rank 0)
                (value
                 ((structure
                   ((id 15) (guards ())
                    (inner
                     (Structure
                      (Arrow
                       (Root
                        ((rank 0)
                         (value
                          ((structure
                            ((id 11) (guards ())
                             (inner (Structure (Constr () string)))
                             (status (Instance <opaque>))))))))
                       (Root
                        ((rank 0)
                         (value
                          ((structure
                            ((id 10) (guards ())
                             (inner (Structure (Constr () string)))
                             (status (Instance <opaque>)))))))))))
                    (status (Instance <opaque>)))))))))
             (Root
              ((rank 0)
               (value
                ((structure
                  ((id 15) (guards ())
                   (inner
                    (Structure
                     (Arrow
                      (Root
                       ((rank 0)
                        (value
                         ((structure
                           ((id 11) (guards ())
                            (inner (Structure (Constr () string)))
                            (status (Instance <opaque>))))))))
                      (Root
                       ((rank 0)
                        (value
                         ((structure
                           ((id 10) (guards ())
                            (inner (Structure (Constr () string)))
                            (status (Instance <opaque>)))))))))))
                   (status (Instance <opaque>))))))))
             (Root
              ((rank 0)
               (value
                ((structure
                  ((id 11) (guards ()) (inner (Structure (Constr () string)))
                   (status (Instance <opaque>))))))))
             (Root
              ((rank 0)
               (value
                ((structure
                  ((id 10) (guards ()) (inner (Structure (Constr () string)))
                   (status (Instance <opaque>))))))))
             (Inner
              (Root
               ((rank 0)
                (value
                 ((structure
                   ((id 18) (guards ())
                    (inner
                     (Structure
                      (Arrow
                       (Root
                        ((rank 1)
                         (value
                          ((structure
                            ((id 19) (guards ())
                             (inner (Structure (Constr () int)))
                             (status (Instance <opaque>))))))))
                       (Root
                        ((rank 1)
                         (value
                          ((structure
                            ((id 19) (guards ())
                             (inner (Structure (Constr () int)))
                             (status (Instance <opaque>)))))))))))
                    (status (Instance <opaque>)))))))))
             (Root
              ((rank 0)
               (value
                ((structure
                  ((id 18) (guards ())
                   (inner
                    (Structure
                     (Arrow
                      (Root
                       ((rank 1)
                        (value
                         ((structure
                           ((id 19) (guards ())
                            (inner (Structure (Constr () int)))
                            (status (Instance <opaque>))))))))
                      (Root
                       ((rank 1)
                        (value
                         ((structure
                           ((id 19) (guards ())
                            (inner (Structure (Constr () int)))
                            (status (Instance <opaque>)))))))))))
                   (status (Instance <opaque>))))))))
             (Root
              ((rank 1)
               (value
                ((structure
                  ((id 19) (guards ()) (inner (Structure (Constr () int)))
                   (status (Instance <opaque>))))))))
             (Inner
              (Root
               ((rank 1)
                (value
                 ((structure
                   ((id 19) (guards ()) (inner (Structure (Constr () int)))
                    (status (Instance <opaque>)))))))))
             (Inner
              (Root
               ((rank 1)
                (value
                 ((structure
                   ((id 2) (guards ()) (inner (Structure (Constr () int)))
                    (status (Instance <opaque>)))))))))))))))
       (path
        ((dst
          ((id 1)
           (parent
            (((id 0) (parent ()) (level 0) (region ((status Alive) (types ()))))))
           (level 1)
           (region
            ((status Alive)
             (types
              ((Root
                ((rank 1)
                 (value
                  ((structure
                    ((id 19) (guards ()) (inner (Structure (Constr () int)))
                     (status (Instance <opaque>))))))))
               (Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 18) (guards ())
                     (inner
                      (Structure
                       (Arrow
                        (Root
                         ((rank 1)
                          (value
                           ((structure
                             ((id 19) (guards ())
                              (inner (Structure (Constr () int)))
                              (status (Instance <opaque>))))))))
                        (Root
                         ((rank 1)
                          (value
                           ((structure
                             ((id 19) (guards ())
                              (inner (Structure (Constr () int)))
                              (status (Instance <opaque>)))))))))))
                     (status (Instance <opaque>))))))))
               (Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 15) (guards ())
                     (inner
                      (Structure
                       (Arrow
                        (Root
                         ((rank 0)
                          (value
                           ((structure
                             ((id 11) (guards ())
                              (inner (Structure (Constr () string)))
                              (status (Instance <opaque>))))))))
                        (Root
                         ((rank 0)
                          (value
                           ((structure
                             ((id 10) (guards ())
                              (inner (Structure (Constr () string)))
                              (status (Instance <opaque>)))))))))))
                     (status (Instance <opaque>))))))))
               (Root
                ((rank 1)
                 (value
                  ((structure
                    ((id 2) (guards ()) (inner (Structure (Constr () int)))
                     (status (Instance <opaque>))))))))
               (Inner
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 15) (guards ())
                      (inner
                       (Structure
                        (Arrow
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 11) (guards ())
                               (inner (Structure (Constr () string)))
                               (status (Instance <opaque>))))))))
                         (Root
                          ((rank 0)
                           (value
                            ((structure
                              ((id 10) (guards ())
                               (inner (Structure (Constr () string)))
                               (status (Instance <opaque>)))))))))))
                      (status (Instance <opaque>)))))))))
               (Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 15) (guards ())
                     (inner
                      (Structure
                       (Arrow
                        (Root
                         ((rank 0)
                          (value
                           ((structure
                             ((id 11) (guards ())
                              (inner (Structure (Constr () string)))
                              (status (Instance <opaque>))))))))
                        (Root
                         ((rank 0)
                          (value
                           ((structure
                             ((id 10) (guards ())
                              (inner (Structure (Constr () string)))
                              (status (Instance <opaque>)))))))))))
                     (status (Instance <opaque>))))))))
               (Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 11) (guards ()) (inner (Structure (Constr () string)))
                     (status (Instance <opaque>))))))))
               (Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 10) (guards ()) (inner (Structure (Constr () string)))
                     (status (Instance <opaque>))))))))
               (Inner
                (Root
                 ((rank 0)
                  (value
                   ((structure
                     ((id 18) (guards ())
                      (inner
                       (Structure
                        (Arrow
                         (Root
                          ((rank 1)
                           (value
                            ((structure
                              ((id 19) (guards ())
                               (inner (Structure (Constr () int)))
                               (status (Instance <opaque>))))))))
                         (Root
                          ((rank 1)
                           (value
                            ((structure
                              ((id 19) (guards ())
                               (inner (Structure (Constr () int)))
                               (status (Instance <opaque>)))))))))))
                      (status (Instance <opaque>)))))))))
               (Root
                ((rank 0)
                 (value
                  ((structure
                    ((id 18) (guards ())
                     (inner
                      (Structure
                       (Arrow
                        (Root
                         ((rank 1)
                          (value
                           ((structure
                             ((id 19) (guards ())
                              (inner (Structure (Constr () int)))
                              (status (Instance <opaque>))))))))
                        (Root
                         ((rank 1)
                          (value
                           ((structure
                             ((id 19) (guards ())
                              (inner (Structure (Constr () int)))
                              (status (Instance <opaque>)))))))))))
                     (status (Instance <opaque>))))))))
               (Root
                ((rank 1)
                 (value
                  ((structure
                    ((id 19) (guards ()) (inner (Structure (Constr () int)))
                     (status (Instance <opaque>))))))))
               (Inner
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 19) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>)))))))))
               (Inner
                (Root
                 ((rank 1)
                  (value
                   ((structure
                     ((id 2) (guards ()) (inner (Structure (Constr () int)))
                      (status (Instance <opaque>)))))))))))))))
         (compare_node_by_level <fun>) (mem <fun>)))
       (mem <fun>))))
    Type made generic
    Type made generic
    Type made generic
    Type made generic
    Type made generic
    Type made generic
    Type made generic
    Type made generic
    Type made generic
    ("Constraint is satisfiable"
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
         (Eq (Var ((id 9) (name Type.Var))) (Structure (Constr () int))))))))
    |}]
;;
