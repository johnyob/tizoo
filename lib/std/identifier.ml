open Core
open Incr

module T = struct
  type t = int [@@deriving equal, compare, sexp, hash, bin_io]
end

include T
include Comparable.Make (T)

type source = { next_id : unit -> t }

let create_source () =
  let next_id = ref 0 in
  { next_id = (fun () -> post_incr next_id) }
;;

let create source = source.next_id ()
