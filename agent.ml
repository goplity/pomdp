open Printf

module List   = ListLabels
module String = StringLabels

type 'a t =
  { space           : 'a Space.t
  ; pos             : int  (* Which coords position this agent handles. *)
  ; max_val         : 'a
  ; max_coord       : int
  ; dim_coord_probs : float list
  }

let make ~pos ~space ~init_max_val ~init_max_coord ~init_dim_coord_probs =
  { pos
  ; space
  ; max_val         = init_max_val
  ; max_coord       = init_max_coord
  ; dim_coord_probs = init_dim_coord_probs
  }

let is_converged {dim_coord_probs; _} ~epsilon =
  List.for_all dim_coord_probs ~f:(fun p ->
    (p < epsilon) || (p > (1.0 -. epsilon))
  )

let coord {max_coord; _} =
  max_coord

let update dim_coord_probs ~chosen ~coefficient:c =
  let (_, dim_coord_probs, sum) =
    List.fold_left dim_coord_probs ~init:(-1, [], 0.0) ~f:(fun (i, ps, sum) p ->
      let i = succ i in
      if i = chosen then
        (i, p :: ps, sum)
      else
        let p = p *. c in
        (i, p :: ps, sum +. p)
    )
  in
  let dim_coord_probs = List.rev dim_coord_probs in
  let p_at_chosen = 1.0 -. sum in
  List.mapi dim_coord_probs ~f:(fun i p -> if i = chosen then p_at_chosen else p)

let uniform () =
  Random.float 1.0

let choose dim_coord_probs =
  let u = uniform () in
  let exception Chosen of int in
  match
    (List.fold_left dim_coord_probs ~init:(-1, 0.0) ~f:(fun (i, sum) p ->
      let i = succ i in
      let sum = sum +. p in
      if sum > u then (raise (Chosen i));
      (i, sum)
    ))
  with
  | exception Chosen i -> i
  | _ -> assert false

let set_nth xs n x =
  List.mapi xs ~f:(fun i x' -> if i = n then x else x')

let next
  ({pos; space; max_val=v0; max_coord=c0; dim_coord_probs} as t)
  ~coords
  ~coefficient
=
  let c1 = choose dim_coord_probs in
  let coords = set_nth coords pos c1 in
  let max_val, max_coord, dim_coord_probs =
    let v1 = Space.get space coords in
    match Space.cmp v1 v0 with
    | `GT  | `EQ ->
        (v1, c1, update dim_coord_probs ~chosen:c1 ~coefficient)
    | `LT ->
        (v0, c0, dim_coord_probs)
  in
  {t with max_val; max_coord; dim_coord_probs}

let eprint
  {pos; space=_; max_val; max_coord; dim_coord_probs}
  ~val_to_string
  ~indent
=
  Printf.eprintf
    "%sagent: %d, max_val: %s, max_coord: %d, dim_coord_probs: [%s]\n%!"
    indent
    pos
    (val_to_string max_val)
    max_coord
    (String.concat ~sep:"; " (List.map dim_coord_probs ~f:(sprintf "%f")))
