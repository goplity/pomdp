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
  let n_trials = ref 1_000 in
  Arg.parse [("-n", Arg.Int (fun n -> n_trials := n), "")] (fun _ -> ()) "";
  let n_trials = !n_trials in

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
  let iter_max = ref 0 in
  let iter_min = ref 1_000_000 in  (* A dirty lie, but simplifies init val *)
  let iter_sum = ref 0 in
  Random.self_init ();
  repeat n_trials (fun () ->
    let Pomdp.({coordinates; iterations; _}) =
      Pomdp.maximize
        init_prob_vecs
        ~trace:false
        ~init:[0; 0]
        ~max:(fun c1 c2 -> if (get c1) >= (get c2) then c1 else c2)
        ~epsillon:0.01
    in
    let max = get coordinates in
    count max;
    if iterations > !iter_max then iter_max := iterations;
    if iterations < !iter_min then iter_min := iterations;
    iter_sum := !iter_sum + iterations
  );
  let iter_mean = !iter_sum / n_trials in
  let counts = Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts [] in
  let counts = List.sort counts ~cmp:(fun (_, v1) (_, v2) -> compare v1 v2) in
  printf "\n";
  printf "Result | Count\n%!";
  printf "-------+------\n%!";
  List.iter counts ~f:(fun (k, v) -> printf "%6d | %5d\n%!" k v);
  printf "==============\n%!";
  printf "         %5d\n%!" n_trials;
  printf "\n";
  printf "Iterations:\n";
  printf "    min  %d\n" !iter_min;
  printf "    max  %d\n" !iter_max;
  printf "    mean %d\n" iter_mean;
  printf "\n"

let () =
  main ()
