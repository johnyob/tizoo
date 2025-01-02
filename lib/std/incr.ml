let pre_incr r =
  incr r;
  !r
;;

let post_incr r =
  let result = !r in
  incr r;
  result
;;
