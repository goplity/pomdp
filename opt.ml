type 'a t = 'a option

let get_assert = function
  | Some x -> x
  | None   -> assert false

let update t ~f ~default =
  match t with
  | None   -> Some default
  | Some x -> Some (f x)
