open Printf

module List   = ListLabels
module String = StringLabels

module Int = struct
  type t = int
  let compare = Stdlib.compare
end
module IntSet = Set.Make (Int)

type 'a state =
  { coords      : Space.coordinates
  ; agents      : ('a Agent.t) list
  ; iterations  : int
  ; converged   : bool
  }

let is_converged agents ~epsilon =
  List.for_all agents ~f:(Agent.is_converged ~epsilon)

let state_print {converged; iterations; agents; coords} ~val_to_string =
  eprintf "iter: %5d, converged: %B, coords: [%s], agents:\n%!"
    iterations
    converged
    (String.concat ~sep:"; " (List.map coords ~f:string_of_int));
  List.iter agents ~f:(Agent.eprint ~val_to_string ~indent:"  ")

let state_next state ~coefficient ~epsilon =
  match state with
  | {converged=true; _} ->
      state
  | {converged=false; iterations; agents; coords=_} ->
      let coords = List.map agents ~f:Agent.coord in
      let agents = List.map agents ~f:(Agent.next ~coords ~coefficient) in
      { iterations = succ iterations
      ; converged  = is_converged agents ~epsilon
      ; agents
      ; coords
      }

let state_init ~spaces ~coords ~epsilon =
  let agents =
    List.mapi
      (List.combine spaces (Space.init_prob_of_indices_per_dim (List.hd spaces)))
      ~f:(fun pos (space, init_dim_coord_probs) ->
          let init_max_val = Space.get space coords in
          (* TODO: Is init_max_coord selection OK? *)
          let init_max_coord = List.nth coords pos in
          Agent.make ~pos ~space ~init_max_val ~init_max_coord ~init_dim_coord_probs
      )
  in
  { iterations = 0
  ; converged  = is_converged agents ~epsilon
  ; agents
  ; coords
  }

let validate spaces =
  let n_spaces = List.length spaces in
  eprintf "n_spaces: %d\n%!" n_spaces;
  (* At least 1 space is given, and all have same lengths per dimension. *)
  let all_rows = List.map spaces ~f:Space.rows in  (* Dim 1 *)
  let all_cols = List.map spaces ~f:Space.cols in  (* Dim 2 *)
  assert (IntSet.cardinal (IntSet.of_list all_rows) = 1);
  assert (IntSet.cardinal (IntSet.of_list all_cols) = 1);
  (* TODO: Remove static dims limit (when other code can support it). *)
  let max_dimensions = 2 in
  assert (n_spaces = max_dimensions)

let maximize
  ?(trace=false)
  ~spaces
  ~space_val_to_string
  ~init_coords_max
  ~coefficient
  ~epsilon
=
  validate spaces;  (* TODO: Return a meaningful error value to the user *)
  let rec iter state =
    match state with
    | {converged=true; _} ->
        state
    | {converged=false; _} ->
        if trace then state_print state ~val_to_string:space_val_to_string;
        iter (state_next state ~coefficient ~epsilon)
  in
  iter (state_init ~spaces ~coords:init_coords_max ~epsilon)
