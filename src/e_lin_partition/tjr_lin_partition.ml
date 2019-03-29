(* A linear partition of a key space; this is used in the B-tree to implement nodes. 


v.3, based on a map from k option to r

From a list r0 k0 r1 k1 ... kn rn+1, we have a partition of the space K into:

- below k0
- k0 <= _ < k1
- k1 <= _ < k2
- ...
- kn <= _

And the corresponding map: fun k -> if k < k0 then r0 else ...



------------

Operations we need to support (see frame_ops and node_ops): 

- find(k); we also need to be able to retrieve the actual key that
  matched k (see below)
- add(intv,r): change the value for interval intv
- merge(intv1,intv2,r): for two adjacent intervals, merge (assuming at
  least one key remains)
- split(intv,r1,k1,r2): split the interval intv (delete old interval,
  add two new intervals)
  - refine_below(r0,k0): split Less_than(k) into Less_than(k0) and Between(k0,k)
  - refine_above(kn,rn): ditto, vice versa
- adjust_midpoint(intv1,intv2,r1,k1,r2)- for steal cases; equivalent
  to deleting two intvs then adding two
- get size, and split into two partitions (subject to some size
  constraints etc on each partition)



*)

let dest_Some x = match x with Some x -> x | _ -> failwith "dest_Some"


let rec iter_opt f x =
  match f x with
  | Some x -> iter_opt f x
  | None -> x


type ('k,'r,'t) partition = ('k option, 'r, 't) Tjr_poly_map.map


(* None is less than any other lower bound; this corresponds to the
   "lowest" interval below k0 *)
let rec key_compare k_cmp k1 k2 =
  match k1,k2 with 
  | None,None -> 0
  | None,_ -> -1
  | _,None -> 1
  | Some k1, Some k2 -> k_cmp k1 k2


(* 
FIXME in the following we want to always be working with a map that maintains internal invariants.

So, given a key k, we can get the range k1 <= k < k2 (or other), and get the corresponding r.

Operations:

| find(k)                               | get intv for k; lookup intv                       | 
| add(intv,r)                           | add/replace intv binding in map                       | 
| merge_adjacent(intv1,intv2,r)                  | remove intvs; add(intv,r); adjust pred/succ       | 
| split(intv,r1,k1,r2)                  | remove intv; add new intvs; adjust pred/succ      | 
| adjust_midpoint(intv1,intv2,r1,k1,r2) | remove two intvs; add two intvs; adjust pred/succ | 
| get size                              | pred/succ.elements gives size                     | 
| split into two                        | map has a split operation, and also set; use these to implement split | 
| merge keyspaces  | take two adjacent kspaces separated by a key, and form a single keyspace; k maps to lower keys of second keyspace
*)

(* Use the term "keyspace"; FIXME better options? FIXME xxx marks interface that is probably not used *)
type ('k,'r,'t) keyspace_ops = {
  krs2ks: 'k list * 'r list -> 't;
  ks2krs: 't -> 'k list * 'r list;

  find_intv_and_binding_for_key: 'k -> 't -> ('k option * 'r); 
  (* NOTE total; kopt is lower bound of intv *)

  k_size: 't -> int;

  split_keyspace_on_key: 'k -> 't -> 't*'k*'t; (* FIXME semantics? *)
  split_keyspace_at_index: int -> 't -> 't*'k*'t; (* FIXME semantics? *)

  merge_keyspaces: 't*'k*'t -> 't;
  ks_dest_cons: 't -> ('r*'k*'t);
  ks_dest_snoc: 't -> ('t*'k*'r);

  ks_internal_empty: 't;
  ks_internal_add: 'k option -> 'r -> 't -> 't;  (* for snoc and cons *)
  (* ks_internal_remove: 'k option -> 't -> 't; *)


(*  
  xxx_split_keyspace: 't -> 't * 'k * 't;
  xxx_empty: 't;
  xxx_bindings: 't -> ('k option * 'r) list;
  xxx_add: 'k option -> 'r -> 't -> 't;
  xxx_merge_adjacent: 'k option -> 'k -> 'r -> 't -> 't;
  (* first arg is lower bound of first intv; second is midpt *)
  xxx_split_intv: 'k option -> 'r*'k*'r -> 't -> 't;
  xxx_adjust_midpoint: 'k option -> 'k -> 'r*'k*'r -> 't -> 't;  
  (* adjust_midpoint l mid : l is the lower bound of the first
     interval; mid is the midpoint *)
*)
}

  

let make_keyspace_ops (type k r t) ~(k_cmp:k -> k -> int) : (k,r,(k option,r,t)Tjr_poly_map.map) keyspace_ops =
  let map_ops : (k option,r,(k option,r,t)Tjr_poly_map.map) Tjr_poly_map.map_ops = Tjr_poly_map.make_map_ops (key_compare k_cmp) in    
  let _ = map_ops in
  let krs2ks (ks,rs) = 
    assert(List.length rs = List.length ks + 1);
    let ks = None::(List.map (fun x -> Some x) ks) in
    map_ops.of_bindings (List.combine ks rs)
  in
  let ks2krs ks = 
    ks |> map_ops.bindings |> List.split |> fun (ks,rs) -> 
    (List.tl ks|>List.map dest_Some), rs
  in
  let _ = ks2krs in
  let find_intv_and_binding_for_key k t = 
    map_ops.split (Some k) t |> fun (t1,v,t2) ->
    match v with
    | None -> (
        (* k is not present; so take max binding in t1 *)
        map_ops.max_binding_opt t1 |> function
        | None -> failwith "impossible, there is always a binding for None, which is less than Some k"
        | Some (l,v) -> (l,v))
    | Some v -> (Some k,v)
  in
  let _ = find_intv_and_binding_for_key in
  let k_size t =
    (* assumes there is at least one binding *)
    map_ops.cardinal t - 1
  in
  let split_keyspace_at_index i t = 
    (* FIXME inefficient *)
    map_ops.bindings t |> fun bs -> 
    assert(List.length bs >= 2);
    Tjr_list.drop (i+1) bs |> function
    | [] -> failwith "impossible"
    | (None,_)::_ -> failwith "impossible"
    | (Some k,v)::_ -> 
      map_ops.split (Some k) t |> fun (t1,_,t2) -> 
      (t1,k,map_ops.add None v t2)
  in
  let _ = split_keyspace_at_index in
  let split_keyspace_on_key k t = 
    find_intv_and_binding_for_key k t |> fun (k',_r) ->
    match k' with
    | None -> failwith __LOC__
    | Some k' -> 
      map_ops.split (Some k') t |> fun (t1,r,t2) -> 
      map_ops.add None (dest_Some r) t2 |> fun t2 ->
      (t1,k', t2)
  in
  let merge_keyspaces (t1,k,t2) = 
    (* as t1, but k maps to the keys below t2.k0; we can alter t2 to
       map k to t2.None (and remove None mapping); then just combine
       the maps *)
    let t2 = 
      let ks = map_ops.find_opt None t2 |> dest_Some in
      t2 |> map_ops.remove None |> map_ops.add (Some k) ks 
    in
    map_ops.disjoint_union t1 t2
  in 
  let _ = merge_keyspaces in
  (* keyspace(r,k,...) -> (r,k,keyspace(...)) *)
  let ks_dest_cons t = 
    let r = map_ops.find_opt None t |> dest_Some in
    let t = map_ops.remove None t in
    let k = map_ops.min_binding_opt t |> dest_Some |> fun (k,v) -> k |> dest_Some in
    let t = map_ops.remove (Some k) t in
    (r,k,t)
  in
  let _ = ks_dest_cons in
  (* keyspace(...,k,r) -> (keyspace(...),k,r) *)
  let ks_dest_snoc t = 
    let k = map_ops.max_binding_opt t |> dest_Some |> fun (k,v) -> k |> dest_Some in
    let r = map_ops.find_opt (Some k) t |> dest_Some in
    let t = map_ops.remove (Some k) t in
    (t,k,r)
  in
  let _ = ks_dest_snoc in
  let ks_internal_add k r t = map_ops.add k r t in
  let ks_internal_empty = map_ops.empty in
  { krs2ks; ks2krs; find_intv_and_binding_for_key; k_size; split_keyspace_on_key; 
    split_keyspace_at_index; merge_keyspaces; ks_dest_cons; ks_dest_snoc; ks_internal_add;
    ks_internal_empty } 



let _ = make_keyspace_ops


  (* some older additional operations *)
(*
  let empty = map_ops.empty in
  let bindings = map_ops.bindings in
  let add k r t = map_ops.add k r t in
  let merge_adjacent k1 k2 r t =
    t 
    |> map_ops.remove (Some k2)
    |> map_ops.add k1 r 
  in
  let split_intv intv (r1,k,r2) t =
    t
    |> map_ops.add intv r1 
    |> map_ops.add (Some k) r2
  in
  let adjust_midpoint low mid (r1,mid',r2) t =
    assert(map_ops.mem low t);
    assert(map_ops.mem (Some mid) t);
    t 
    |> map_ops.remove (Some mid)
    |> map_ops.add low r1
    |> map_ops.add (Some mid') r2
  in
*)
      


(* test ------------------------------------------------------------- *)

module Test(_ : sig end) : sig end = struct
  let ops = make_keyspace_ops ~k_cmp:Tjr_int.compare

  let ex = ops.krs2ks (
    [2;4;6],
    ["r0";"r1";"r2";"r3"])

  let _ = ex |> ops.ks2krs

  let _ = ex |> ops.find_intv_and_binding_for_key 1
  let _ = ex |> ops.find_intv_and_binding_for_key 2
  let _ = ex |> ops.find_intv_and_binding_for_key 7

(*
  let _ = ex |> ops.add (Some 5) "r" |> ops.bindings

  let _ = ex |> ops.merge_adjacent (Some 4) 6 "r" |> ops.bindings

  let _ = ex |> ops.split_intv (Some 4) ("r6",5,"r7") |> ops.bindings

  let _ = ex |> ops.adjust_midpoint (Some 2) 4 ("a",3,"b") |> ops.bindings
*)

  let _ = ex |> ops.k_size

(*
  let _ = ex |> ops.split_keyspace |> fun (t1,k,t2) ->
          (ops.bindings t1, k, ops.bindings t2)
*)
  ;;
end
