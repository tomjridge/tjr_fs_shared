include Kv_op_type


let op2k = function
  | Insert (k,_v) -> k
  | Delete k -> k

(* let op2s op = op |> op_to_yojson |> Yojson.Safe.pretty_to_string  *)

let ii_op2s (op:(int,int)op) = 
  op 
  |> op_to_yojson (fun (i:int) -> `Int i) (fun (i:int) -> `Int i) 
  |> Yojson.Safe.pretty_to_string 



(** The type for the abstract view of the DCL. Also required by the
   make_dcl_ops function. NOTE the values are ('k,'v)op, not 'v. *)
type ('k,'v,'map) kvop_map_ops = ('k,('k,'v)op,'map) Tjr_map.map_ops

(* FIXME default kv map, not kvop *)
let default_kvop_map_ops () : ('k,'v,'map) kvop_map_ops = 
  let open Tjr_poly_map in
  let map_ops = make_map_ops Pervasives.compare in
  let open Tjr_map in
  { map_empty=map_ops.empty;
    map_is_empty=map_ops.is_empty;
    map_add=map_ops.add;
    map_remove=map_ops.remove;
    map_find=map_ops.find_opt;
    map_bindings=map_ops.bindings}
