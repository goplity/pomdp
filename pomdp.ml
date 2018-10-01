module List = ListLabels

module Order = struct
  type t = Lt | Eq | Gt

  type 'a compare = ('a -> 'a -> t)

  let compare x y =
    match Stdlib.compare x y with
    | 0            -> Eq
    | n when n < 0 -> Lt
    | n when n > 0 -> Gt
    | _            -> assert false
end

module Pomdp : 
sig
  type prob = float

  type prob_index = int

  type coordinates = prob_index list

  val maximize :
    (prob list) list 
    -> init        :'a
    -> get         : (coordinates -> 'a)
    -> compare     : 'a Order.compare
    -> coefficient : float
    -> 'a
end = struct
  type prob = float

  type prob_index = int

  type coordinates = prob_index list

  let uniform () =
    Random.float 1.0

  let choose prob_vecs =
    List.map prob_vecs ~f:(fun prob_vec ->
      let u = uniform () in 
      let exception Return of int in
      match
        (List.fold_left prob_vec ~init:(-1, 0.0) ~f:(fun (i, sum) p ->
          let i = succ i in
          let sum = sum +. p in
          if sum > u then (raise (Return i));
          (i, sum)
        ))
      with
      | exception Return i -> i
      | _ -> assert false
    )
  
  let update prob_vecs ~coordinates ~coefficient:c =
    let update prob_vec index =
      let (_, prob_vecs, sum) =
        List.fold_left prob_vec ~init:(0, [], 0.0) ~f:(fun (i, ps, sum) p ->
          if i = index then
            (succ i, p :: ps, sum)
          else
            let p = p *. c in
            (succ i, p :: ps, sum +. p)
        )
      in
      let prob_vecs = List.rev prob_vecs in
      let p_at_index = 1.0 -. sum in
      List.mapi prob_vecs ~f:(fun i p -> if i = index then p_at_index else p)
    in
    List.map2 prob_vecs coordinates ~f:update

  let is_converged prob_vecs ~epsillon =
    List.for_all prob_vecs ~f:(List.for_all ~f:(fun p ->
      (p < epsillon) || (p > (1.0 -. epsillon))
    ))

  let maximize prob_vecs ~init:max ~get ~compare ~coefficient =
    let rec iter prob_vecs max =
      if is_converged prob_vecs ~epsillon:coefficient then
        max
      else
        let coordinates = choose prob_vecs in
        let candidate_max = get coordinates in
        let (prob_vecs, max) =
          match compare max candidate_max with
          | Order.Gt | Order.Eq ->
              (prob_vecs, max)
          | Order.Lt ->
              let prob_vecs = update prob_vecs ~coordinates ~coefficient in
              let max = candidate_max in
              (prob_vecs, max)
        in
        iter prob_vecs max
    in
    iter prob_vecs max
end

let main () =
  let space =
    [ [7; 2]
    ; [0; 7]
    ]
  in
  let init_max  = 0 in
  let init_prob _ = 0.5 in
  let init_prob_vec_r = List.map space              ~f:init_prob in
  let init_prob_vec_k = List.map (List.nth space 0) ~f:init_prob in
  let init_prob_vecs =
    [ init_prob_vec_r
    ; init_prob_vec_k
    ]
  in
  let get = function
    | [r; k] -> List.nth (List.nth space r) k
    | _      -> assert false
  in
  let max =
    Pomdp.maximize
      init_prob_vecs
      ~init:        init_max
      ~get
      ~compare:     Order.compare
      ~coefficient: 0.01
  in
  Printf.printf "max: %d\n" max

let () =
  main ()
