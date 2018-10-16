open Printf

module Array = ArrayLabels
module List  = ListLabels

module Opt : sig
  type 'a t = 'a option

  val update : 'a t -> f:('a -> 'b) -> default:'b -> 'b t

  val get_assert : 'a t -> 'a
end = struct
  type 'a t = 'a option

  let get_assert = function
    | Some x -> x
    | None   -> assert false

  let update t ~f ~default =
    match t with
    | None   -> Some default
    | Some x -> Some (f x)
end

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
  ; trace    : bool
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
  let trace    = ref false in
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
  let iter_max = ref None in
  let iter_min = ref None in
  let iter_sum = ref 0 in
  let time_max = ref None in
  let time_min = ref None in
  let time_sum = ref 0.0 in
  Random.self_init ();
  repeat opt.n_trials (fun () ->
    let t0 = Sys.time () in
    let Pomdp.({coordinates; iterations; _}) =
      Pomdp.maximize
        ~prob_vecs:init_prob_vecs
        ~trace:opt.trace
        ~init:[0; 0]
        ~cmp:(fun c1 c2 ->
            match compare (get c1) (get c2) with
            | n when n < 0 -> `LT
            | n when n > 0 -> `GT
            | _            -> `EQ
        )
        ~coefficient:opt.coefficient
        ~epsilon:opt.epsilon
    in
    let t1 = Sys.time () in
    let time = t1 -. t0 in
    let max = get coordinates in
    count max;
    iter_max := Opt.update !iter_max ~f:(Stdlib.max iterations) ~default:iterations;
    iter_min := Opt.update !iter_min ~f:(Stdlib.min iterations) ~default:iterations;
    iter_sum := !iter_sum + iterations;
    time_max := Opt.update !time_max ~f:(Stdlib.max time) ~default:time;
    time_min := Opt.update !time_min ~f:(Stdlib.min time) ~default:time;
    time_sum := !time_sum +. time
  );
  let iter_max = Opt.get_assert !iter_max in
  let iter_min = Opt.get_assert !iter_min in
  let iter_mean = !iter_sum / opt.n_trials in
  let time_max  = Opt.get_assert !time_max in
  let time_min  = Opt.get_assert !time_min in
  let time_sum  = !time_sum in
  let time_mean = time_sum /. (float_of_int opt.n_trials) in
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
  printf "Time:\n";
  printf "    %f\n" time_sum;
  printf "\n";
  printf "Iterations per trial:\n";
  printf "    min  %d\n" iter_min;
  printf "    max  %d\n" iter_max;
  printf "    mean %d\n" iter_mean;
  printf "\n";
  printf "Time per trial:\n";
  printf "    min  %f\n" time_min;
  printf "    max  %f\n" time_max;
  printf "    mean %f\n" time_mean;
  printf "\n"

let () =
  main ()
