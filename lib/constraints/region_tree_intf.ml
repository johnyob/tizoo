open! Import

module Level : sig
  type t = private int [@@deriving equal, compare, sexp, hash]

  include Comparable.S with type t := t

  val init : t
  val zero : t
  val enter : t -> t
  val exit : t -> t
end = struct
  module T = struct
    type t = int [@@deriving equal, compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let init = -1
  let zero = 0
  let enter t = t + 1
  let exit t = t - 1
end

module type T1_with_sexp_of = sig
  type 'a t [@@deriving sexp_of]
end

module type S = sig
  type 'a region

  (** A region tree constraints a tree of regions. The level of a node within the tree
      represents the node's depth. *)
  type 'a node =
    { id : Identifier.t (** Unique identifier of the region node *)
    ; parent : 'a node option
    (** Parent of the node, if [None] then node is a root node. *)
    ; level : Level.t
    (** The level of the node in the region tree.
        If [parent] is [None], then [level = Level.zero],
        otherwise [level = Level.enter parent.level]. *)
    ; region : 'a region (** The region of the node *)
    }
  [@@deriving sexp_of]

  and 'a t = T of 'a node [@@unboxed] [@@deriving sexp_of]

  (** [root t] returns the root node of the region tree *)
  val root : 'a t -> 'a node

  (** [create ~id_source ~region] returns a new region tree *)
  val create : id_source:Identifier.source -> region:'a region -> 'a t

  (** [create_node ~id_source ~parent ~region] returns a new region node with parent [parent] *)
  val create_node
    :  id_source:Identifier.source
    -> parent:'a node
    -> region:'a region
    -> 'a node

  (** [nearest_common_ancestor n1 n2] returns the nearest common ancestor of two nodes in a region tree *)
  val nearest_common_ancestor : 'a node -> 'a node -> 'a node

  val unsafe_max_by_level : 'a node list -> 'a node

  module Path : sig
    (** A (linear) path in the region tree from the root to a given node *)
    type 'a t

    (** [of_node n] returns a path from the region tree root to the node [n] *)
    val of_node : 'a node -> 'a t

    (** [compare_node_by_level p] is a total ordering on nodes in the path [p] *)
    val compare_node_by_level : 'a t -> 'a node -> 'a node -> int

    (** [mem p n] returns whether the node [n] is in the path [p] *)
    val mem : 'a t -> 'a node -> bool

    (** [dst p] returns the destination node of the path [p] *)
    val dst : 'a t -> 'a node
  end
end

module type Intf = sig
  module Level = Level

  module type S = S

  module Make (R : T1_with_sexp_of) : S with type 'a region := 'a R.t
end
