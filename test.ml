open Printf

module List = ListLabels

let rec repeat n thunk =
  if n <= 0 then () else (thunk (); repeat (pred n) thunk)

let main () =
  let n_trials = 1000 in
  let space =
    [ [1; 2; 3]
    ; [4; 5; 6]
    ; [7; 8; 9]
    ]
  in
  let rows = space in
  let col0 = List.nth space 0 in
  let n_rows = List.length rows in
  let n_cols = List.length col0 in
  let init_prob_vecs =
    let one_out_of total = 1.0 /. (float_of_int total) in
    [ List.map rows ~f:(fun _ -> one_out_of n_rows)
    ; List.map col0 ~f:(fun _ -> one_out_of n_cols)
    ]
  in
  let get = function
    | [r; k] -> List.nth (List.nth space r) k
    | _      -> assert false
  in
  let counts = Hashtbl.create (n_rows * n_cols) in
  List.iter space ~f:(List.iter ~f:(fun x -> Hashtbl.replace counts x 0));
  let count x = Hashtbl.replace counts x (succ (Hashtbl.find counts x)) in
  Random.self_init ();
  repeat n_trials (fun () ->
    let coordinates_of_max =
      Pomdp.maximize
        ~trace:false
        init_prob_vecs
        ~init:      [1; 0]
        ~compare:   (fun c1 c2 -> Order.compare (get c1) (get c2))
        ~coefficient: 0.1
    in
    let max = get coordinates_of_max in
    count max
  );
  printf "Result | Count\n%!";
  printf "-------+------\n%!";
  Hashtbl.iter
    (fun k v -> printf "%6d | %5d\n%!" k v)
    counts;
  printf "==============\n%!";
  printf "         %5d\n%!" n_trials

let () =
  main ()
