include Core
include Mlsus_std
module Unifier = Mlsus_unifier.Unifier

module Identifier : sig
  type t = private int [@@deriving equal, compare, sexp, hash]

  include Comparable.S with type t := t

  type source

  val create_source : unit -> source
  val create : source -> t
end = struct
  module T = struct
    type t = int [@@deriving equal, compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  type source = { next_id : unit -> t }

  let create_source () =
    let next_id = ref 0 in
    { next_id = (fun () -> post_incr next_id) }
  ;;

  let create source = source.next_id ()
end
