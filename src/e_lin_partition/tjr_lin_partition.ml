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

Operations we need to support: 

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
So, given a key k, we can get the range k1 <= k < k2 (or other), and get the corresponding r.

Operations:

| find(k)                               | get intv for k; lookup intv                       | 
| add(intv,r)                           | replace intv binding in map                       | 
| merge_adjacent(intv1,intv2,r)                  | remove intvs; add(intv,r); adjust pred/succ       | 
| split(intv,r1,k1,r2)                  | remove intv; add new intvs; adjust pred/succ      | 
| adjust_midpoint(intv1,intv2,r1,k1,r2) | remove two intvs; add two intvs; adjust pred/succ | 
| get size                              | pred/succ.elements gives size                     | 
| split into two                        | map has a split operation, and also set; use these to implement split | 

*)

(* Use the term "keyspace"; FIXME better options? *)
type ('k,'r,'t) keyspace_ops = {
  find: 'k -> 't -> ('k option * 'r); (* NOTE total; kopt is lower bound of intv *)
  add: 'k option -> 'r -> 't -> 't;
  merge_adjacent: 'k option -> 'k -> 'r -> 't -> 't;
  split_intv: 'k option -> 'r*'k*'r -> 't -> 't;
  adjust_midpoint: 'k option -> 'k -> 'r*'k*'r -> 't -> 't;  
  (* adjust_midpoint l mid : l is the lower bound of the first
     interval; mid is the midpoint *)
  k_size: 't -> int;
  split_keyspace: 't -> 't * 'k * 't;
}

  

let make_keyspace_ops ~(k_cmp:'k -> 'k -> int) : ('k,'r,'t)keyspace_ops =
  let map_ops = Tjr_poly_map.make_map_ops (key_compare k_cmp) in    
  let find k t = 
    map_ops.split (Some k) t |> fun (t1,v,t2) ->
    match v with
    | None -> (
        (* k is not present; so take max binding in t1 *)
        map_ops.max_binding_opt t1 |> function
        | None -> failwith "impossible, there is always a binding for None, which is less than Some k"
        | Some (l,v) -> (l,v))
    | Some v -> (Some k,v)
  in
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
    t 
    |> map_ops.remove (Some mid)
    |> map_ops.add low r1
    |> map_ops.add (Some mid') r2
  in
  let k_size t =
    (* assumes there is at least one binding *)
    map_ops.cardinal t - 1
  in
  let split_keyspace t = 
    map_ops.bindings t |> fun bs -> 
    assert(List.length bs >= 2);
    let l = (List.length bs)/2 in
    Tjr_list.drop l bs |> function
    | [] -> failwith "impossible"
    | (None,_)::_ -> failwith "impossible"
    | (Some k,v)::_ -> 
      map_ops.split (Some k) t |> fun (t1,_,t2) -> 
      (t1,k,map_ops.add None v t2)
  in
  { find; add; merge_adjacent; split_intv; adjust_midpoint; k_size; split_keyspace } 
  

let _ = make_keyspace_ops
      
