open Printf

module Find :
sig
  type prob =
    { r : float
    ; k : float
    }

  val max
    : get:(r:int -> k:int -> int)
    -> max:int
    -> prob:prob
    -> spaces:(int array * int array)
    -> epsillon:float
    -> int * int

  val choose : int array -> float -> int
end = struct
  type prob =
    { r : float
    ; k : float
    }

  let uniform () =
    Random.float 1.0

  let choose _ p_of_choosing_0 =
    if (uniform ()) < p_of_choosing_0 then
      0
    else
      1

  let update (p : float) (i : int) (coefficient : float) : float =
    let c = coefficient in
    match i with
    | 0 -> min 1.0 (  c *. p)
    | 1 -> max 0.0 (1.0 -. c *. (1.0 -. p))
    | _ -> assert false

  let prob_to_str {r; k} =
    let pct x = int_of_float (floor (x *. 100.0)) in
    let pct_r, pct_k = pct r, pct k in
    sprintf
      "p_r: (%3d, %3d), p_k: (%3d, %3d)"
      pct_r (100 - pct_r) pct_k (100 - pct_k)

  let is_converged p epsillon =
    (p > (1.0 -. epsillon)) || (p < epsillon)

  let call = ref 0
  let rec max ~get ~max:m ~prob ~spaces ~epsillon:e =
    let {r=p_r; k=p_k} = prob in
    incr call;
    let (space_r, space_k) = spaces in
    let r = choose space_r p_r in
    let k = choose space_k p_k in
    let candidate_max = get ~r ~k in
    (*
    printf
      "call: %5d, %s, r: %d, k: %d, candidate_max: %d\n%!"
      !call      (prob_to_str prob)  r      k      candidate_max;
    *)
    let z = function 0 -> -1 | x -> x in
    printf "%d %d\n%!" (z r) (z k);
    let prob, m =
      if candidate_max >= m then
        let new_prob =
          { r = update p_r r 1.001
          ; k = update p_k k 1.001
          }
        in
        (new_prob, candidate_max)
      else
        (prob, m)
    in
    match prob with
    | {r=p_r; k=p_k} when (is_converged p_r e) && (is_converged p_k e) ->
        (r, k)
    | _ ->
        max ~get ~max:m ~prob ~spaces ~epsillon:e
end

let get m ~r ~k =
  m.(r).(k)

let main () =
  let m =
    [| [| 7; 2 |]
    ;  [| 0; 7 |]
    |]
  in

  let space_r = Array.mapi (fun i _ -> i) m in
  let space_k = Array.mapi (fun i _ -> i) m.(0) in

  let (r, k) =
    Find.max
      ~get:      (get m)
      ~max:      (-1)
      ~prob:     Find.({r = 0.5; k = 0.5})
      ~spaces:   (space_r, space_k)
      ~epsillon: 0.05
  in

  let value = get m ~r ~k in
  ignore value

  (*print_endline (String.make 80 '-');*)
  (*Printf.printf "r: %d, k: %d, value: %d\n" r k value*)

let () =
  main ()
