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

module Stats : sig
  type num =
    [`Int of int | `Float of float]

  type t

  val create : unit -> t

  val count : t -> num -> unit

  val report : t -> unit
end = struct
  type num =
    [`Int of int | `Float of float]

  type t =
    { mutable min   : float option
    ; mutable max   : float option
    ; mutable sum   : float option
    ; mutable count : int
    }

  let create () =
    { min   = None
    ; max   = None
    ; sum   = None
    ; count = 0
    }

  let count t n =
    let n = match n with `Float n -> n | `Int n -> float_of_int n in
    t.min   <- Opt.update t.min ~default:n ~f:(Stdlib.min n);
    t.max   <- Opt.update t.max ~default:n ~f:(Stdlib.max n);
    t.sum   <- Opt.update t.sum ~default:n ~f:((+.) n);
    t.count <- succ t.count

  let report {min; max; sum; count} =
    (* TODO: Use int where original value was int *)
    let min = Opt.get_assert min in
    let max = Opt.get_assert max in
    let sum = Opt.get_assert sum in
    printf "    min   : %f\n" min;
    printf "    max   : %f\n" max;
    printf "    sum   : %f\n" sum;
    printf "    count : %d\n" count;
    printf "    mean  : %f\n" (sum /. (float_of_int count));
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

let matrix_print m ~to_string ~indent =
  Array.iter m ~f:(fun row ->
    eprintf "%s" indent;
    let sep = ref "" in
    Array.iter row ~f:(fun x ->
      eprintf "%s%s%!" !sep (to_string x);
      sep := " "
    );
    eprintf "\n%!"
  )

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
  eprintf "\nGenerated space:\n%!";
  matrix_print matrix ~to_string:(fun i -> sprintf "%3d" i) ~indent:"  ";
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
  let stat_iter = Stats.create () in
  let stat_time = Stats.create () in
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
    Stats.count stat_iter (`Int iterations);
    Stats.count stat_time (`Float time);
  );
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
  printf "\n";
  printf "Iterations per trial:\n";
  Stats.report stat_iter;
  printf "\n";
  printf "Time per trial:\n";
  Stats.report stat_time;
  printf "\n"

let () =
  main ()
