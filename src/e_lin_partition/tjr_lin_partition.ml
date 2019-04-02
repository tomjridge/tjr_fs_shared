(* A linear partition of a key space; this is used in the B-tree to implement nodes. 


We have two main types:
- keyspace: a finite, total map from some totally ordered keyspace
- semi-keyspace: as keyspace, but covers only those keys >= some given key (the "lower bound")

The implementation of keyspace and semi-keyspace both use k option as the key, rather than k. This
allows the use of "None" as a "lower bound" for the whole key space, and also allows a keyspace to be split easily into two halfs. The semi-keyspace does not have a "None" entry of course, so the typing is looser than it could be (in favour of more efficient operations).

The node of a B-tree has pointers r separated by keys k. Take k0=None.

Then we have:

k0<= f(r0) < k1 <= f(r1) ...

From a list r0 k1 r1 k2 ... kn rn, we have a partition of the space K into:

- below k1
- k1 <= _ < k2
- ...
- kn <= _

And the corresponding map: fun k -> if k < k1 then r0 else ...

For the operations we need to support, see frame_ops and node_ops

*)

(* \doc(old) *)

let dest_Some x = match x with Some x -> x | _ -> failwith "dest_Some"


let rec iter_opt f x =
  match f x with
  | Some x -> iter_opt f x
  | None -> x


(* If min_key is None, the map is total; otherwise the map is
   a semi-keyspace, defined above min_key *)
type ('k,'r,'t) partition = ('k option, 'r, 't) Tjr_poly_map.map


(* None is less than any other lower bound; this corresponds to the
   "lowest" interval below k0 *)
let rec key_compare k_cmp k1 k2 =
  match k1,k2 with 
  | None,None -> 0
  | None,_ -> -1
  | _,None -> 1
  | Some k1, Some k2 -> k_cmp k1 k2


(* Use the term "keyspace"; FIXME better options? FIXME xxx marks
   interface that is probably not used 

See \doc(keyspace_ops)
*)
type ('k,'r,'t) keyspace_ops = {
  krs2ks: 'k list * 'r list -> 't;  (* NOTE assumes |rs|=|ks|+1 *)
  ks2krs: 't -> 'k list * 'r list;  (* NOTE |rs|=|ks|+1; no None key *)

  find_intv_and_binding_for_key: 'k -> 't -> ('k option * 'r); 
  (* NOTE if t is total, then this is total; kopt is greatest lower
     bound for k; it is an error to call this on a semi-keyspace? *)

  k_size: 't -> int;

  split_keyspace_on_key: 'k -> 't -> 't*'k option*'r*'t; (* see doc *)

  split_keyspace_at_index: int -> 't -> 't*'k*'t; 
  (* only for total keyspace; returns two *total* keyspaces; see doc *)

  merge_keyspaces: 't*'k*'t -> 't;  (* see doc *)
  
  ks_dest_cons: 't -> ('r*'k*'t);  
  (* steal right; expected to be used on total keyspace; see doc *)

  ks_dest_snoc: 't -> ('t*'k*'r);  (* see doc *)
}

(*


  (* reveal internals ?*)
  ks_internal_ops: ('k option,'r,('k option,'r,'t)Tjr_poly_map.map) Tjr_poly_map.map_ops

  ks_internal_empty: 't;
  ks_internal_add: 'k option -> 'r -> 't -> 't;  (* for snoc and cons *)
  ks_internal_find_opt: 'k option -> 't -> 'r option;  (* lookup directly in map *)

  ks_dest_cons: 't -> ('r*'k*'t);  (* expected to be used on semi-keyspace? *)
  ks_dest_snoc: 't -> ('t*'k*'r);  (* also for semi-keyspace *)
*)
  

let make_keyspace_ops' (type k r t) ~(k_cmp:k -> k -> int) =
  let map_ops : (k option,r,(k option,r,t)Tjr_poly_map.map) Tjr_poly_map.map_ops = Tjr_poly_map.make_map_ops (key_compare k_cmp) in    
  let _ = map_ops in
  let is_total t = 
    match map_ops.min_binding_opt t with Some (None,_) -> true | _ -> false
  in
  let krs2ks (ks,rs) = 
    assert(List.length rs = List.length ks + 1);
    let ks = None::(List.map (fun x -> Some x) ks) in
    map_ops.of_bindings (List.combine ks rs)
  in
  let ks2krs ks = 
    assert (is_total ks);
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
      map_ops.split (Some k) t |> fun (t1,_(*Some v*),t2) -> 
      (t1,k,map_ops.add None v t2)
  in
  let _ = split_keyspace_at_index in
  let split_keyspace_on_key k t = 
    find_intv_and_binding_for_key k t |> fun (k',r) ->
    map_ops.split k' t |> fun (t1,r',t2) -> 
    (* this gets us ...^k(i-1)_r(i), with k'=ki, and t2 as rh *)
    assert (r'=Some r);
    (* DONT add a lower bound to t2; see doc *)
    (t1,k',r, t2)
  in
  let merge_keyspaces (t1,k,t2) = (* see doc *)
    let t2 = 
      assert (is_total t1);
      assert (is_total t2);
      (* k is lower bound for t2; strict upper bound for t1 *)
      let ks = map_ops.find_opt None t2 |> dest_Some in
      t2 |> map_ops.remove None |> map_ops.add (Some k) ks 
    in
    map_ops.disjoint_union t1 t2
  in 
  let _ = merge_keyspaces in
  let ks_dest_cons t = 
    assert (is_total t);
    let r0 = map_ops.find_opt None t |> dest_Some in
    let k1,r1 = map_ops.min_binding_opt t |> dest_Some in
    assert(k1<>None);
    let k1 = k1 |> dest_Some in
    let t = t|>map_ops.add None r1 in
    (r1,k1,t)
  in
  let _ = ks_dest_cons in
  let ks_dest_snoc t = 
    assert (is_total t);
    let kn,rn = map_ops.max_binding_opt t |> dest_Some in
    assert (kn<>None);
    let kn = kn |> dest_Some in
    let t = map_ops.remove (Some kn) t in
    (t,kn,rn)
  in
(*  let _ = ks_dest_snoc in
  let ks_internal_add k r t = map_ops.add k r t in
  let ks_internal_empty = map_ops.empty in
  let ks_internal_find_opt k t = map_ops.find_opt k t in *)
  let keyspace_ops : (k,r,(k option,r,t)Tjr_poly_map.map) keyspace_ops = 
    { krs2ks; ks2krs; find_intv_and_binding_for_key; k_size; split_keyspace_on_key; 
    split_keyspace_at_index; merge_keyspaces; ks_dest_cons; ks_dest_snoc; 
    (* ks_internal_add; ks_internal_empty; ks_internal_find_opt  *)
    }
  in
  fun f -> f ~keyspace_ops ~map_ops


let _ : 
k_cmp:('a -> 'a -> int) ->
(keyspace_ops:('a, 'b, ('a option, 'b, 'c) Tjr_poly_map.map) keyspace_ops ->
 map_ops:('a option, 'b, ('a option, 'b, 'c) Tjr_poly_map.map)
         Tjr_poly_map.map_ops ->
 'd) ->
'd
= make_keyspace_ops'


(* hide the polymap in the type ------------------------------------- *)

type internal

(* this reveals the structure of the keyspace *)
type ('k,'r) keyspace = ('k option,'r,internal) Tjr_poly_map.map

module Internal : sig
  open Tjr_poly_map
  val make_keyspace_ops : k_cmp:('a -> 'a -> int) ->
    ('a, 'b, ('a, 'b) keyspace) keyspace_ops
  val make_map_ops: k_cmp:('k -> 'k -> int) ->
    ('k option, 'r, ('k,'r)keyspace) map_ops
end = struct
  let make_keyspace_ops ~k_cmp = make_keyspace_ops' ~k_cmp @@ fun ~keyspace_ops ~map_ops -> 
    keyspace_ops

  let make_map_ops ~k_cmp = make_keyspace_ops' ~k_cmp @@ fun ~keyspace_ops ~map_ops -> 
    map_ops
end

include Internal



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

(* old ks ops

ks_internal_remove: 'k option -> 't -> 't; 
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
