module Var = Var
module Union_find = Union_find

let post_incr r =
  let result = !r in
  incr r;
  result
;;
