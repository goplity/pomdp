open Printf

module Array = ArrayLabels
module List  = ListLabels

type gen_spec =
  { r : int
  ; k : int
  ; order : [`inc | `dec | `ran]
  }

type opt =
  { n_trials : int
  ; data_src : [ `read | `gen of gen_spec ]
  ; epsilon  : float
  ; coefficient : float
  }

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

let gen {r; k; order} =
  let next =
    match order with
    | `inc ->
        let count = ref 0 in
        fun () -> incr count; !count
    | `dec ->
        let count = ref (r * k) in
        fun () -> decr count; !count
    | `ran ->
        fun () -> Random.int 1000
  in
  let matrix = Array.make_matrix ~dimx:r ~dimy:k 0 in
  Array.iteri matrix ~f:(fun r row -> Array.iteri row ~f:(fun k _ ->
    matrix.(r).(k) <- next ()
  ));
  Array.iteri matrix ~f:(fun r row -> eprintf "\n%!"; Array.iteri row ~f:(fun k _ ->
      eprintf "%d %!" matrix.(r).(k);));
  eprintf "\n";
  Array.to_list (Array.map matrix ~f:Array.to_list)

let rec repeat n thunk =
  if n <= 0 then () else (thunk (); repeat (pred n) thunk)

let opt () : opt =
  let epsilon  = ref 0.01 in
  let coefficient = ref 0.9 in
  let n_trials = ref 1_000 in
  let n_rows   = ref 3 in
  let n_cols   = ref 3 in
  let data_src = ref `read in
  let data_src_gen order =
    data_src := `gen {r = !n_rows; k = !n_cols; order}
  in
  Arg.parse (Arg.align
    [ ( "-gen"
      , Arg.Tuple
          [ Arg.Set_int n_rows
          ; Arg.Set_int n_cols
          ; Arg.Symbol
              ( ["inc"; "dec"; "ran"]
              , (function
                | "inc" -> data_src_gen `inc
                | "dec" -> data_src_gen `dec
                | "ran" -> data_src_gen `ran
                | _     -> assert false
                )
              )
          ]
      , " Generate data (instead of reading): <rows> <cols> <inc|dec|ran>"
      )
    ; ("-n", Arg.Set_int n_trials     , " Number of trials to run")
    ; ("-e", Arg.Set_float epsilon    , " Epsilon")
    ; ("-c", Arg.Set_float coefficient, " Coefficient")
    ])
    (fun _ -> ())
    "";
  { n_trials = !n_trials
  ; data_src = !data_src
  ; epsilon  = !epsilon
  ; coefficient = !coefficient
  }

let main () =
  let opt = opt () in
  let space =
    match opt.data_src with
    | `read     -> read_ints ()
    | `gen spec -> gen spec
  in

  let rows = space in
  let row0 = List.nth rows 0 in
  let n_rows = List.length rows in
  let n_cols = List.length row0 in
  let n_elements = n_rows * n_cols in
  let init_prob_vecs =
    let one_out_of total = 1.0 /. (float_of_int total) in
    [ List.map rows ~f:(fun _ -> one_out_of n_rows)
    ; List.map row0 ~f:(fun _ -> one_out_of n_cols)
    ]
  in
  let get = function
    | [r; k] -> List.nth (List.nth space r) k
    | _      -> assert false
  in
  let counts = Hashtbl.create n_elements in
  List.iter space ~f:(List.iter ~f:(fun x -> Hashtbl.replace counts x 0));
  let count x = Hashtbl.replace counts x (succ (Hashtbl.find counts x)) in
  let iter_max = ref 0 in
  let iter_min = ref 1_000_000 in  (* A dirty lie, but simplifies init val *)
  let iter_sum = ref 0 in
  Random.self_init ();
  repeat opt.n_trials (fun () ->
    let Pomdp.({coordinates; iterations; _}) =
      Pomdp.maximize
        ~prob_vecs:init_prob_vecs
        ~trace:true
        ~init:[0; 0]
        ~max:(fun c1 c2 -> 
            let val1 = (get c1) in
            let val2 = (get c2) in
            match compare val1 val2 with
            | -1 -> `lt 
            | 0 -> `eq 
            | 1 -> `gt
        )
        ~coefficient:opt.coefficient
        ~epsilon:opt.epsilon
    in
    let max = get coordinates in
    count max;
    if iterations > !iter_max then iter_max := iterations;
    if iterations < !iter_min then iter_min := iterations;
    iter_sum := !iter_sum + iterations
  );
  let iter_mean = !iter_sum / opt.n_trials in
  let counts = Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts [] in
  let counts = List.sort counts ~cmp:(fun (_, v1) (_, v2) -> compare v1 v2) in
  let rank =
    let tbl = Hashtbl.create n_elements in
    List.iteri
      (List.sort counts ~cmp:(fun (k1, _) (k2, _) -> compare k2 k1))
      ~f:(fun r (k, _) -> Hashtbl.replace tbl k (r + 1));  (* Because 0 index *)
    fun x ->
      Hashtbl.find tbl x
  in
  printf "\n";
  printf "Result | Count | Percent | Rank\n%!";
  printf "-------+-------+---------+-------\n%!";
  List.iter counts ~f:(fun (element, count) ->
    printf
      "%6d | %5d |    %3d%% | %5d\n%!"
      element count (100 * count / opt.n_trials) (rank element)
  );
  printf "=================================\n%!";
  printf "\n";
  printf "Trials:\n";
  printf "    %d\n" opt.n_trials;
  printf "\n";
  printf "Iterations per trial:\n";
  printf "    min  %d\n" !iter_min;
  printf "    max  %d\n" !iter_max;
  printf "    mean %d\n" iter_mean;
  printf "\n"

let () =
  main ()
