module List = ListLabels

module Pomdp : 
sig
  type prob = float

  type prob_index = int

  type coordinates = prob_index list

  type order = Lt | Eq | Gt

  val maximize :
    (prob list) list 
    -> init:'a 
    -> get: (coordinates -> 'a)
    -> compare: ('a -> 'a -> order)
    -> coefficient:float
    -> coordinates
end = struct
  type prob = float

  type prob_index = int

  type coordinates = prob_index list

  type order = Lt | Eq | Gt

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

  let maximize prob_vecs ~init:max ~get ~compare ~coefficient =
    let coordinates = choose prob_vecs in
    let candidate_max = get coordinates in
    let (prob_vecs, max) =
      match compare max candidate_max with
      | Gt
      | Eq -> (prob_vecs       , max)
      | Lt -> (update prob_vecs ~coordinates ~coefficient, candidate_max)
    in
    (*choose prob_vecs*)
    (* TODO: Decide: return or recur *)
end
