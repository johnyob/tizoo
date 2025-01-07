open! Core
open! Grace
open Mlsus_main

let () =
  let open Async.Log.Global in
  For_testing.use_test_output ()
;;

let type_check_and_print
  ?(dump_ast = false)
  ?(dump_constraint = false)
  ?(log_level = `Info)
  str
  =
  Async.Log.Global.set_level log_level;
  let source = `String { Source.name = Some "expect_test.ml"; content = str } in
  type_check_and_print
    ~source
    ~dump_ast
    ~dump_constraint
    (Lexing.from_string ~with_positions:true str)
;;

let include_fix = "external fix : 'a 'b. (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b;;"

let include_list =
  {|
    type 'a list =
      | Nil
      | Cons of 'a * 'a list
    ;;
  |}
;;

let include_option =
  {|
    type 'a option = 
      | None 
      | Some of 'a 
    ;;
  |}
;;

let%expect_test "" =
  let str =
    include_fix
    ^ {|
      let power = fix (fun power x n -> 
          if n = 0 
            then 1
            else x * power x (n - 1) 
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ {|
      external mod : int -> int -> int;;

      let even = fun n -> mod n 2 = 0;;

      let power = fix (fun power x n ->
          if n = 1 
            then x
            else if even n 
              then power (x * x)  (n / 2)
              else x * power (x * x) (n / 2)
        )
      ;; 
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ {|
      let sum = 
        fix (fun sum n -> 
          if n = 0 then 0 
          else n + sum (n - 1))
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ {|
      let sum = fun n ->
        let loop = fix (fun loop n acc ->
          if n = 0 then acc
          else loop (n - 1) (n + acc))
        in loop n
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      let mem = fix (fun mem t x equal ->
        match t with
        ( Nil -> false
        | Cons (y, t) -> 
          if equal x y then true 
          else mem t x equal 
        ))
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      let zip = 
        fix (fun zip t1 t2 ->
          match (t1, t2) with
          ( (Cons (x1, t1), Cons (x2, t2)) ->
              Cons ((x1, x2), zip t1 t2) 
          | _ -> Nil  
          ))
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      let unzip = 
        fix (fun unzip t ->
          match t with
          ( Nil -> (Nil, Nil)
          | Cons ((x1, x2), t) ->
            let t1t2 = unzip t in
            match t1t2 with (
              (t1, t2) -> (Cons (x1, t1), Cons (x2, t2))
            )   
          )
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      external raise_no_more_coins : 'a. unit -> 'a;; 
      
      let change = 
        fix (fun change till amt ->
          match (till, amt) with
          ( (_, 0) -> Nil
          | (Nil, _) -> raise_no_more_coins ()
          | (Cons (c, till), amt) ->
            if amt < c then change till amt
            else Cons (c, change (Cons (c, till)) (amt - c) )     
          )
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      external append : 'a. 'a list -> 'a list -> 'a list;;

      let change = 
        fix (fun change till amt ->
          match (till, amt) with
          ( (_, 0) -> Cons (Nil, Nil)
          | (Nil, _) -> Nil
          | (Cons (c, till), amt) ->
            if amt < c then change till amt
            else 
              let loop = fix (fun loop t -> 
                  match t with
                  ( Nil -> Nil
                  | Cons (cs, css) -> Cons (Cons (c, cs), loop css)
                  )
                )
              in
                append 
                  (loop (change (Cons (c, till)) (amt - c)))
                  (change till amt)  
          )
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      let change = 
        fix (fun change till amt ->
          let loop = fix 
            (fun loop till amt chg chgs ->
              match (till, amt) with
              ( (_, 0) -> Cons (chg, chgs)
              | (Nil, _) -> chgs
              | (Cons (c, till), amt) ->
                  if amt < 0 then chgs
                  else
                    loop (Cons (c, till)) (amt - c) (Cons (c, chg)) (loop till amt chg chgs) 
              )
            )
          in loop till amt Nil Nil
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    {|
      type vehicle = 
        | Bike
        | Motorbike
        | Car
        | Lorry
      ;;

      let m = Motorbike;;

      let wheels = 
        fun t -> 
          match t with
          ( Bike -> 2
          | Motorbike -> 2
          | Car -> 4
          | Lorry -> 18
          )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    {|
      type vehicle = 
        | Bike
        | Motorbike of int (* engine size in CCs *)
        | Car of bool (* true if a Reliant Robin *)
        | Lorry of int (* number of wheels *)
      ;;

      let wheels = 
        fun t ->
          match t with
          ( Bike -> 2
          | Motorbike _ -> 2
          | Car is_robin -> if is_robin then 3 else 4
          | Lorry w -> w
          )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str = include_option ^ {|
      let x = Some 1;;

      let y = None;;
    |} in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      external raise_no_change : 'a. int -> 'a;; 
      external try_with_no_change : 'a. (unit -> 'a) -> (int -> 'a) -> 'a;;

      let change = 
        fix (fun change till amt ->
          match (till, amt) with
          ( (_, 0) -> Nil
          | (Nil, amt) -> raise_no_change amt
          | (Cons (c, till), amt) ->
              if amt < c 
                then raise_no_change amt
                else try_with_no_change 
                      (fun () -> Cons (c, change (Cons (c, till)) (amt - c)))
                      (fun _ -> change till amt)
          )
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    {|
      type shape = 
        | Null
        | Circle of int (* radius *)
        | Join of shape * shape
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      type 'a tree = 
        | Lf
        | Br of 'a tree * 'a * 'a tree
      ;;

      external append : 'a. 'a list -> 'a list -> 'a list;;

      let pre_order = 
        fix (fun pre_order t ->
          match t with
          ( Lf -> Nil
          | Br (l, x, r) ->
            append (Cons (x, Nil)) 
              (append (pre_order l) (pre_order r))
          )
        )
      ;;

      let in_order = 
        fix (fun in_order t ->
          match t with
          ( Lf -> Nil
          | Br (l, x, r) ->
            append (pre_order l)
              (append (Cons (x, Nil)) (pre_order r))
          )
        )
      ;;

      let post_order = 
        fix (fun post_order t ->
          match t with
          ( Lf -> Nil
          | Br (l, x, r) ->
            append (post_order l)
              ( append (post_order r) (Cons (x, Nil)) )  
          )
        )
      ;;

      let in_order = fun t ->
        let loop = 
          fix (fun loop t acc ->
            match t with
            ( Lf -> acc
            | Br (l, x, r) -> 
              loop l (Cons (x, loop r acc))
            )
          )
        in loop t
      ;;

      let pre_order = fun t ->
        let loop = 
          fix (fun loop t acc ->
            match t with
            ( Lf -> acc
            | Br (l, x, r) ->
              Cons (x, loop l (loop r acc))
            )
          )
        in loop t
      ;;

      let post_order = fun t ->
        let loop = 
          fix (fun loop t acc ->
            match t with
            ( Lf -> acc
            | Br (l, x, r) ->
              loop l (loop r (Cons (x, acc)))  
            )
          )
        in loop t
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_list
    ^ {|
      let a1 = 
        Cons (fun n -> n * 2, Cons (fun n -> n * 3, Cons (fun n -> n + 1, Nil)))
      ;;

      let a2 = 
        fun n -> n * 2
      ;;

      let a3 = 
        (fun n -> n * 2) 17
      ;;

      let double = fun n -> n * 2;;

      let a4 = 
        fun x -> match x with (0 -> true | _ -> false)
      ;;

      let is_zero = 
        fun x -> match x with (0 -> true | _ -> false)
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      external map : 'a 'b. ('a -> 'b) -> 'a list -> 'b list;;
      external hd : 'a. 'a list -> 'a;;
      external tl : 'a. 'a list -> 'a list;;

      let transpose = 
        fix (fun transpose t ->
          match t with
          ( Cons (Nil, _) -> Nil
          | rows ->
            Cons (map hd rows, transpose (map tl rows))
          )
        )
      ;;

      let dot_product = 
        fix (fun dot_product xs ys ->
          match (xs, ys) with
          ( (Nil, Nil) -> 0
          | (Cons (x, xs), Cons (y, ys)) ->
              (x * y) + dot_product xs ys
          )
        )
      ;;


      let product = 
        fun a b ->
          let c = transpose b in
          map (fun rows -> map (dot_product rows) c) a
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ {|
      type 'a tree = 
        | Lf
        | Br of 'a tree * 'a * 'a tree
      ;;

      let cons = 
        fix (fun cons t x ->
          match t with
          ( Lf -> Br (Lf, x, Lf)
          | Br (l, y, r) ->
              Br (cons l y, x, r)  
          )
        )
      ;;
      
      external invalid_arg : 'a. unit -> 'a;;

      let uncons =
        fix (fun uncons t -> 
          match t with
          ( Lf -> invalid_arg ()
          | Br (Lf, x, Lf) -> (x, Lf)
          | Br (l, x, r) ->
            match uncons l with (
              (y, l') -> (x, Br (r, x, l'))
            )
          )
        )
      ;;

      let hd = fun t ->
        match uncons t with ((x, _) -> x)
      ;;

      let tl = fun t ->
        match uncons t with ((_, t) -> t)
      ;;

      external mod : int -> int -> int;;

      let even = fun n -> mod n 2 = 0;;

      let nth = 
        fix (fun nth t n ->
          match (t, n) with
          ( (Lf, _) -> invalid_arg ()
          | (Br (_, x, _), 0) -> x
          | (Br (l, x, r), n) ->
              if even n then nth r (n / 2)
              else nth l (n / 2)
          )
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      type 'a tree = 
        | Lf
        | Br of 'a tree * 'a * 'a tree
      ;;

      external raise_empty : 'a. unit -> 'a;;

      type 'a queue = Q of 'a list * 'a list;;
      let empty = Q (Nil, Nil);;

      let is_empty = fun q ->
        match q with
        ( Q (Nil, Nil) -> true
        | _ -> false)
      ;;

      external rev : 'a. 'a list -> 'a list;;

      let norm = fun q ->
        match q with
        ( Q (Nil, ys) -> Q (rev ys, Nil)
        | q -> q
        )
      ;;

      let enqueue = fun (Q (xs, ys)) y -> norm (Q (xs, Cons (y, ys)));;
      let dequeue = fun q ->
        match q with
        ( Q (Cons (x, xs), ys) -> norm (Q (xs, ys))
        | _ -> raise_empty ()
        )
      ;;

      let hd = fun q ->
        match q with
        ( Q (Cons (x, _), _) -> x
        | _ -> raise_empty ()
        )
      ;;

      let bfs = 
        fix (fun bfs q -> 
          if is_empty q then Nil
          else
            match hd q with
            ( Lf -> bfs (dequeue q)
            | Br (l, x, r) ->
              Cons (x, bfs (enqueue (enqueue (dequeue q) l) r) ) 
            )
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    include_fix
    ^ include_list
    ^ {|
      type 'a seq = 
        | Seq_nil
        | Seq_cons of 'a * (unit -> 'a seq)
      ;;

      external raise_empty : 'a. unit -> 'a;;

      let hd = fun t ->
        match t with
        ( Seq_cons (x, _) -> x
        | _ -> raise_empty ()
        )
      ;;

      let tl = fun t ->
        match t with
        ( Seq_cons (_, tf) -> tf ()
        | _ -> raise_empty ()
        )
      ;;

      let empty = Seq_nil ;;

      let is_empty = fun t ->
        match t with
        ( Seq_nil -> true
        | _ -> false
        )
      ;;

      let map = 
        fix (fun map f t ->
          match t with
          ( Seq_nil -> Seq_nil
          | Seq_cons (x, tf) -> Seq_cons (f x, fun () -> map f (tf ()))
          ) 
        )
      ;;

      let filter = 
        fix (fun filter f t ->
          match t with
          ( Seq_nil -> Seq_nil
          | Seq_cons (x, tf) ->
              if f x then
                Seq_cons (x, fun () -> filter f (tf ()))
              else
                filter f (tf ())   
          )
        )
      ;;

      let append = 
        fix (fun append t1 t2 ->
          match t1 with
          ( Seq_nil -> t2
          | Seq_cons (x, t1f) ->
              Seq_cons (x, fun () -> append (t1f ()) t2)  
          ) 
        )
      ;;

      let interleave = 
        fix (fun interleave t1 t2 ->
          match t1 with
          ( Seq_nil -> t2
          | Seq_cons (x, t1f) ->
              Seq_cons (x, fun () -> interleave t2 (t1f ()))  
          )
        )
      ;;

      let binary_string = 
        fix (fun binary_string bits ->
          Seq_cons (bits, fun () -> 
            interleave
              (binary_string (Cons (0, bits)))
              (binary_string (Cons (1, bits))))
        )
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str = {|
      let id = fun x -> y ;;
    |} in
  type_check_and_print str;
  [%expect {|
    ("Unbound variable"
     (var
      ((it y)
       (range
        ((start 25) (stop 26)
         (source
          (String
           ((name (expect_test.ml))
            (content  "\
                     \n      let id = fun x -> y ;;\
                     \n    ")))))))))
    |}]
;;

let%expect_test "" =
  let str =
    {|
      (* val id : ('a -> 'a as 'a) -> 'a -> 'a *)
      let id = exists (type 'a) ->
        (fun x -> x : 'a -> 'a -> 'a)
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    {|
      let app_error = fun x ->
        (x, x) (fun y -> y)
      ;;
    |}
  in
  type_check_and_print str;
  [%expect
    {|
    ("Failed to solve constraint"
     (err
      (Cannot_unify
       (Arrow (Var ((id 3) (name Decoded_type.Var)))
        (Var ((id 2) (name Decoded_type.Var))))
       (Tuple
        ((Var ((id 0) (name Decoded_type.Var)))
         (Var ((id 1) (name Decoded_type.Var))))))))
    |}]
;;

let%expect_test "" =
  let str = {|
      let x =
        (fun y z -> y z) ()
      ;;
    |} in
  type_check_and_print str;
  [%expect
    {|
    ("Failed to solve constraint"
     (err
      (Cannot_unify
       (Arrow (Var ((id 1) (name Decoded_type.Var)))
        (Var ((id 0) (name Decoded_type.Var))))
       (Constr () ((id 2) (name Stdlib.unit))))))
    |}]
;;

let%expect_test "" =
  let str =
    {|
      type t = 
        | A
      ;; 

      type u = 
        | A
      ;; 

      let x = (A : t) ;;
      let y = (A : u) ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    {|
      type t = 
        | A
      ;; 

      type u = 
        | A
      ;; 

      let z = A ;;
  |}
  in
  type_check_and_print str;
  [%expect
    {|
    (num_partially_generalized_regions(num_partially_generalized_regions 1))
    ("Failed to solve constraint" (err Cannot_resume_match_due_to_cycle))
    |}]
;;

let%expect_test "" =
  let str =
    {|    
      type r = 
        | K of int 
      ;; 

      type s = 
        | K of int 
      ;; 

      let a = 
        fun old ->
          let g = fun x -> 1 + old (K x) in
          (g 0, (old : r -> int))
      ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;

let%expect_test "" =
  let str =
    {|    
      type m = 
        | L 
      ;; 

      type n = 
        | L 
      ;; 

      let x1 = (fun (z : m) -> 1) L ;; 

      let y1 = fun z -> (z : m -> int) L ;;

      let z1 = (fun z -> match z with (L -> 1)) (L : m) ;;
    |}
  in
  type_check_and_print str;
  [%expect {| Well typed :) |}]
;;
