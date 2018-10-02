open Printf

module List = ListLabels

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
  printf "%d\n" max

let () =
  main ()
