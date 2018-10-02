open Printf

module List   = ListLabels
module String = StringLabels


type prob = float

type prob_vecs = (prob list) list

type prob_index = int

type coordinates = prob_index list

type state =
  { coordinates : coordinates
  ; prob_vecs   : prob_vecs
  ; iterations  : int
  }


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
    let (_, prob_vec, sum) =
      List.fold_left prob_vec ~init:(-1, [], 0.0) ~f:(fun (i, ps, sum) p ->
        let i = succ i in
        if i = index then
          (i, p :: ps, sum)
        else
          let p = p *. c in
          (i, p :: ps, sum +. p)
      )
    in
    let prob_vec = List.rev prob_vec in
    let p_at_index = 1.0 -. sum in
    List.mapi prob_vec ~f:(fun i p -> if i = index then p_at_index else p)
  in
  List.map2 prob_vecs coordinates ~f:update

let is_converged prob_vecs ~epsilon =
  List.for_all prob_vecs ~f:(List.for_all ~f:(fun p ->
    (p < epsilon) || (p > (1.0 -. epsilon))
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

let maximize ?(trace=false) prob_vecs ~init:coordinates ~max ~epsilon =
  let coefficient = 1.0 -. epsilon in
  let rec iter ({iterations; prob_vecs; coordinates} as state) =
    let iterations = succ iterations in
    let converged = is_converged prob_vecs ~epsilon in
    if trace then
      eprintf "iter: %5d, probs: %s, coordinates: %s, converged: %B\n%!"
        iterations
        (prob_vecs_to_string prob_vecs)
        (coordinates_to_string coordinates)
        converged;
    if converged then
      state
    else
      let coordinates = max coordinates (choose prob_vecs) in
      let prob_vecs = update prob_vecs ~coordinates ~coefficient in
      iter {iterations; prob_vecs; coordinates}
  in
  iter {iterations = 0; prob_vecs; coordinates}
