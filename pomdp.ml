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
      List.fold_left prob_vec ~init:(-1, [], 0.0) ~f:(fun (i, ps, sum) p ->
        let i = succ i in
        if i = index then
          (i, p :: ps, sum)
        else
          let p = p *. c in
          (i, p :: ps, sum +. p)
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

let coordinates_to_string coordinates =
  (String.concat ~sep:"; " (List.map coordinates ~f:string_of_int))

let maximize ?(trace=false) prob_vecs ~init:coordinates ~compare ~coefficient =
  let rec iter iter_count prob_vecs coordinates =
    let iter_count = succ iter_count in
    let converged = is_converged prob_vecs ~epsillon:coefficient in
    if trace then
      eprintf "iter: %5d, probs: %s, coordinates: %s, converged: %B\n%!"
        iter_count
        (prob_vecs_to_string prob_vecs)
        (coordinates_to_string coordinates)
        converged;
    if converged then
      coordinates
    else
      let coordinates_candidate = choose prob_vecs in
      let (prob_vecs, coordinates) =
        match compare coordinates coordinates_candidate with
        | Order.Gt | Order.Eq ->
            (prob_vecs, coordinates)
        | Order.Lt ->
            let prob_vecs = update prob_vecs ~coordinates ~coefficient in
            (prob_vecs, coordinates_candidate)
      in
      iter iter_count prob_vecs coordinates
  in
  iter 0 prob_vecs coordinates
