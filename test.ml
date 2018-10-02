open Printf

module List = ListLabels

let rec read_lines () =
  match read_line () with
  | exception End_of_file ->
      []
  | line ->
      line :: read_lines ()

let re_whitespace =
  Str.regexp "[ \t]+"

let read_ints () =
  read_lines ()
  |> List.map ~f:(Str.split re_whitespace)
  |> List.map ~f:(List.map ~f:int_of_string)

let rec repeat n thunk =
  if n <= 0 then () else (thunk (); repeat (pred n) thunk)

let main () =
  let n_trials = 1_000 in
  let space = read_ints () in
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
        init_prob_vecs
        ~trace:false
        ~init:[0; 0]
        ~max:(fun c1 c2 -> if (get c1) >= (get c2) then c1 else c2)
        ~epsillon:0.01
    in
    let max = get coordinates_of_max in
    count max
  );
  let counts = Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts [] in
  let counts = List.sort counts ~cmp:(fun (_, v1) (_, v2) -> compare v1 v2) in
  printf "Result | Count\n%!";
  printf "-------+------\n%!";
  List.iter counts ~f:(fun (k, v) -> printf "%6d | %5d\n%!" k v);
  printf "==============\n%!";
  printf "         %5d\n%!" n_trials

let () =
  main ()
