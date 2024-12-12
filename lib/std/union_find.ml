open Base

(* This module implements an imperative data structure for disjoint sets
   (commonly known as `union-find'), based on Tarjan and Huet.

   Union find implements a family of disjoint sets on values, where
   the disjoint sets can dynamically be combined using [union].

   A disjoint set [D] is a family of disjoint sets [D = {t1, ..., tn}],
   with the following operations:
   - [create v]: creates a new set [t] containing [v] in [D].
   - [find v] returns the (unique) set [t] in [D] that contains [v].
   - [union t1 t2] performs the union of [t1] and [t2] in [D].

   A disjoint set [D] is represeted using a forest, a collection of trees,
   each node in the tree storing it's value, with pointers to parents.

   Operations:
   - [find v]:
     This traverses the element [v] back to the root [r] of the set,
     creating a path [p] (the `find' path).

   Path compression is performed on this operation, which updates the
   parent pointer to point directly to the root [r].

   - [union t1 t2]:
     We use union by rank. Each set stores the `rank', an upper bound for the
     height of the tree. The set with the smallest rank becomes the child,
     with the edge case of equal ranks.

   This implementation is optimized for the representation of equivalent
   classes. Each equivalence class containing a "value".
*)

(* Trees representing equivalence classes are of the form:
   {v
            Root
             |
           Inner
        / .. | .. \
     Inner Inner Inner
      /|\   /|\   /|\
      ...   ...   ...
   v}

   With directed edges towards the parents.
   The root of the class contains the [rank] and value of type ['a].
   Internal nodes contain a pointer to their parent.
*)

type 'a root =
  { mutable rank : int
  ; mutable value : 'a
  }

and 'a node =
  | Root of 'a root
  | Inner of 'a t

and 'a t = 'a node ref [@@deriving sexp_of]

let invariant _ t =
  let rec loop t height =
    match !t with
    | Root r -> assert (height <= r.rank)
    | Inner t -> loop t (height + 1)
  in
  loop t 0
;;

let create v = ref (Root { rank = 0; value = v })

(* [compress t ~imm_desc ~imm_desc_node ~prop_descs] compresses the path
   from [t] upwards to the root of [t]'s tree, where:
   - [imm_desc] is the immediate descendent of [t], and
   - [prop_descs] are proper descendents of [imm_desc]

   The use of [imm_desc_node] is to avoid additional heap allocation
   when performing path compression.
*)
let rec compress t ~imm_desc ~imm_desc_node ~prop_descs =
  match !t with
  | Root r ->
    (* Perform path compression *)
    List.iter prop_descs ~f:(fun t -> t := imm_desc_node);
    (* Return pointer to root and it's contents *)
    t, r
  | Inner t' as imm_desc_node ->
    compress t' ~imm_desc:t ~imm_desc_node ~prop_descs:(imm_desc :: prop_descs)
;;

let repr t =
  match !t with
  | Root r -> t, r
  | Inner t' as imm_desc_node -> compress t' ~imm_desc:t ~imm_desc_node ~prop_descs:[]
;;

let root t =
  match !t with
  | Root r -> r
  | _ -> snd (repr t)
;;

let get t = (root t).value
let set t v = (root t).value <- v

let union t1 t2 =
  let t1, r1 = repr t1 in
  let t2, r2 = repr t2 in
  if phys_equal t1 t2
  then ()
  else (
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2
    then t1 := Inner t2
    else (
      t2 := Inner t1;
      if n1 = n2 then r1.rank <- r1.rank + 1))
;;

let same_class t1 t2 = phys_equal (root t1) (root t2)

let is_root t =
  match !t with
  | Root _ -> true
  | Inner _ -> false
;;
