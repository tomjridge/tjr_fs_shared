(* A map that allows:

- for a key not in the map, to get the binding for the greatest lower bound key, and least upper bound key
- for a key in the map, to get the binding for the lowest key greater than that key, and greatest key lower than that key (ie the key neighbours)

This can be simplified:

- for a key (in the map or not), get the next higher binding, or next lower binding

*)

open Tjr_map

let make_ops ~map_ops =
  let k_cmp = map_ops.k_cmp in
  let get_next_binding k t =     
    t |> map_ops.find_first_opt (fun k' -> k_cmp k k' < 0)
  in
  let get_prev_binding k t = 
    t |> map_ops.find_last_opt (fun k' -> k_cmp k' k < 0)
  in
  fun f -> f ~get_next_binding ~get_prev_binding
