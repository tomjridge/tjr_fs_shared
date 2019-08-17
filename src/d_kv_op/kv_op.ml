include Kv_op_type

let op2k = function
  | Insert (k,_v) -> k
  | Delete k -> k

let kvop_to_key = op2k

(* let op2s op = op |> op_to_yojson |> Yojson.Safe.pretty_to_string  *)

let ii_op2s (op:(int,int)kvop) = 
  op 
  |> kvop_to_yojson (fun (i:int) -> `Int i) (fun (i:int) -> `Int i) 
  |> Yojson.Safe.pretty_to_string 

(** The type for the abstract view of the DCL. Also required by the
   make_dcl_ops function. NOTE the values are ('k,'v)op, not 'v. *)
type ('k,'v,'map) kvop_map_ops = ('k,('k,'v)kvop,'map) Tjr_map.map_ops

(* FIXME default kv map, not kvop *)
let default_kvop_map_ops () : ('k,'v,'map) kvop_map_ops = 
  let open Tjr_map in
  let map_ops = make_map_ops Pervasives.compare in
  map_ops

let _ = default_kvop_map_ops

open Tjr_map

let list_to_map ~kvop_map_ops kvops = 
  kvops 
  |> List.map (fun kvop -> (kvop_to_key kvop,kvop))
  |> kvop_map_ops.of_bindings




(*
  let open Tjr_map in
  { map_empty=map_ops.empty;
    map_is_empty=map_ops.is_empty;
    map_add=map_ops.add;
    map_remove=map_ops.remove;
    map_find=map_ops.find_opt;
    map_bindings=map_ops.bindings}
*)
