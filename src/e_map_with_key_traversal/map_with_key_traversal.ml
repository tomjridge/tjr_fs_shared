(* A map that allows:

- for a key not in the map, to get the binding for the greatest lower bound key, and least upper bound key
- for a key in the map, to get the binding for the lowest key greater than that key, and greatest key lower than that key (ie the key neighbours)

This can be simplified:

- for a key (in the map or not), get the next higher binding, or next lower binding

*)

open Tjr_poly_map

let make_ops ~map_ops =
  let get_next_binding k t = 
    map_ops.split k t |> fun (_,_,t) ->
    map_ops.min_binding_opt t
  in
  let get_prev_binding k t = 
    map_ops.split k t |> fun (t,_,_) ->
    map_ops.max_binding_opt t
  in
  fun f -> f ~get_next_binding ~get_prev_binding
