open Printf

module Array = ArrayLabels
module List  = ListLabels

type 'a t = 'a array array

type index = int

type coordinates = index list

type gen_spec =
  { r     : int
  ; k     : int
  ; order : [`inc | `dec | `ran]
  }

let iter t ~f =
  Array.iter t ~f:(Array.iter ~f)

let size t =
  (Array.length t) * (Array.length t.(0))

let init_prob_of_indices_per_dim t =
  let n_rows = Array.length t     in
  let n_cols = Array.length t.(0) in
  let one_out_of total = 1.0 /. (float_of_int total) in
  [ Array.to_list (Array.map t     ~f:(fun _ -> one_out_of n_rows))
  ; Array.to_list (Array.map t.(0) ~f:(fun _ -> one_out_of n_cols))
  ]

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
let t = Array.make_matrix ~dimx:r ~dimy:k 0 in
Array.iteri t ~f:(fun r row -> Array.iteri row ~f:(fun k _ ->
  t.(r).(k) <- next ()
));
t

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
  |> List.map ~f:(Array.of_list)
  |> Array.of_list

let from = function
  | `read     -> read_ints ()
  | `gen spec -> gen spec

let get t = function
  | [r; k] -> t.(r).(k)
  | _      -> assert false

let print t ~to_string ~indent =
  Array.iter t ~f:(fun row ->
    eprintf "%s" indent;
    let sep = ref "" in
    Array.iter row ~f:(fun x ->
      eprintf "%s%s%!" !sep (to_string x);
      sep := " "
    );
    eprintf "\n%!"
  )
