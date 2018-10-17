open Printf

module List = ListLabels

type opt =
  { n_trials : int
  ; data_src : [ `read | `gen of Space.gen_spec ]
  ; epsilon  : float
  ; coefficient : float
  ; trace    : bool
  }

let rec repeat n thunk =
  if n <= 0 then () else (thunk (); repeat (pred n) thunk)

let opt () : opt =
  let trace    = ref false in
  let epsilon  = ref 0.01 in
  let coefficient = ref 0.9 in
  let n_trials = ref 1_000 in
  let n_rows   = ref 3 in
  let n_cols   = ref 3 in
  let data_src = ref `read in
  let data_src_gen order =
    data_src := `gen {Space.r = !n_rows; k = !n_cols; order}
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
    ; ("-t", Arg.Set       trace      , " Trace enabled")
    ])
    (fun _ -> ())
    "";
  { n_trials = !n_trials
  ; data_src = !data_src
  ; epsilon  = !epsilon
  ; coefficient = !coefficient
  ; trace    = !trace
  }

let main opt =
  let space = Space.from opt.data_src in
  eprintf "\nSpace:\n%!";
  Space.print space ~to_string:(fun i -> sprintf "%3d" i) ~indent:"  ";
  let counts = Hashtbl.create (Space.size space) in
  Space.iter space ~f:(fun x -> Hashtbl.replace counts x 0);
  let count x = Hashtbl.replace counts x (succ (Hashtbl.find counts x)) in
  let stat_iter = Stats.create () in
  let stat_time = Stats.create () in
  Random.self_init ();
  repeat opt.n_trials (fun () ->
    let t0 = Sys.time () in
    let Pomdp.({coordinates; iterations; _}) =
      Pomdp.maximize
        ~prob_vecs:(Space.init_prob_of_indices_per_dim space)
        ~trace:opt.trace
        ~init:[0; 0]
        ~cmp:(fun c1 c2 ->
            let v1 = Space.get space c1 in
            let v2 = Space.get space c2 in
            match compare v1 v2 with
            | n when n < 0 -> `LT
            | n when n > 0 -> `GT
            | _            -> `EQ
        )
        ~coefficient:opt.coefficient
        ~epsilon:opt.epsilon
    in
    let t1 = Sys.time () in
    let time = t1 -. t0 in
    let max = Space.get space coordinates in
    count max;
    Stats.count stat_iter (`Int iterations);
    Stats.count stat_time (`Float time);
  );
  let counts = Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts [] in
  let counts = List.sort counts ~cmp:(fun (_, v1) (_, v2) -> compare v1 v2) in
  let rank =
    let tbl = Hashtbl.create (Space.size space) in
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
  printf "Time:\n";
  printf "\n";
  printf "Iterations per trial:\n";
  Stats.report stat_iter;
  printf "\n";
  printf "Time per trial:\n";
  Stats.report stat_time;
  printf "\n"

let () =
  main (opt ())
