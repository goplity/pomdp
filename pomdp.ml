open Printf

module List   = ListLabels
module String = StringLabels


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

let prob_vecs_to_string prob_vecs =
  sprintf "| %s |"
    (String.concat ~sep:" | " (
      List.map prob_vecs ~f:(fun probs ->
        String.concat ~sep:" " (List.map probs ~f:(sprintf "%f"))
      )
    ))

let maximize prob_vecs ~init:max ~get ~compare ~coefficient =
  let rec iter iter_count prob_vecs max =
    let iter_count = succ iter_count in
    eprintf
      "%5d) %s\n%!"
      iter_count
      (prob_vecs_to_string prob_vecs);
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
      iter iter_count prob_vecs max
  in
  iter 0 prob_vecs max
