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
  let open Tjr_polymap in
  let open Tjr_map in
  { map_empty=empty Pervasives.compare;
    map_is_empty=is_empty;
    map_add=add;
    map_remove=remove;
    map_find=find_opt;
    map_bindings=bindings}

(* FIXME this is to map to a "normal" map where values are 'v; but we
   probably want to map to a kvop map *)
(*
let op_list_to_map ops = 
  List.fold_left
    (fun map op -> 
       match op with
       | Insert(k,v) -> Tjr_polymap.add k v map
       | Delete k -> Tjr_polymap.remove k map)
    (Tjr_polymap.empty Pervasives.compare)
    ops
*)
