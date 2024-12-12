open Core

module Make (X : sig
    val module_name : string
  end) : sig
  type t = private
    { id : int
    ; name : string
    }

  val create : ?name:string -> unit -> t

  include Identifiable.S with type t := t
end = struct
  module T = struct
    type t =
      { id : int
      ; name : string
      }
    [@@deriving equal, compare, hash, bin_io, sexp]

    let create =
      let next_id = ref 0 in
      fun ?(name = X.module_name) () ->
        next_id := !next_id + 1;
        { id = !next_id; name }
    ;;

    let of_string s = s |> Sexp.of_string |> t_of_sexp
    let to_string t = t |> sexp_of_t |> Sexp.to_string

    include X
  end

  include T
  include Identifiable.Make (T)
end
