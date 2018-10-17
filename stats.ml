open Printf

type num =
  [`Int of int | `Float of float]

type t =
  { mutable min   : float option
  ; mutable max   : float option
  ; mutable sum   : float option
  ; mutable count : int
  }

let create () =
  { min   = None
  ; max   = None
  ; sum   = None
  ; count = 0
  }

let count t n =
  let n = match n with `Float n -> n | `Int n -> float_of_int n in
  t.min   <- Opt.update t.min ~default:n ~f:(Stdlib.min n);
  t.max   <- Opt.update t.max ~default:n ~f:(Stdlib.max n);
  t.sum   <- Opt.update t.sum ~default:n ~f:((+.) n);
  t.count <- succ t.count

let report {min; max; sum; count} =
  (* TODO: Use int where original value was int *)
  let min = Opt.get_assert min in
  let max = Opt.get_assert max in
  let sum = Opt.get_assert sum in
  printf "    min   : %f\n" min;
  printf "    max   : %f\n" max;
  printf "    sum   : %f\n" sum;
  printf "    count : %d\n" count;
  printf "    mean  : %f\n" (sum /. (float_of_int count));
