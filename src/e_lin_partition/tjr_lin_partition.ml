(* A linear partition of a key space; this is used in the B-tree to implement nodes. 


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

It may be simpler to implement these operations via conversion to
lists? But this seems a bit awful.

For the steal/merge-left cases, we need to be able to navigate to the
immediately-preceding interval. And similarly, we need to be able to
navigate to the immediately-following interval. Given an interval, the
immediately-following interval can be derived. However, for the
immediately preceeding interval, we probably have to maintain a "pred"
map. This could be done in-map (eg as extra info in the value
component of a map entry), or outside (as a separate map that is
updated independently). Alternatively, we could accept a "key" that is
semantically "slightly less than" some given key. This would allow to
locate the "previous" interval fairly directly.

At any rate, we probably need:

- next_intv(intv): intv option
- prev_intv(intv): intv option
*)

(* FIXME should be called "interval"? *)
type 'k key = 
  | Less_than of 'k  (* lt *)
  | Between of 'k * 'k  (* k1 <= _ < k2 *)
  | Greater_eq of 'k  (* k1 <= _ *)
  | Find of 'k  (* this is only for searching; should never appear in the domain of the map *)

type ('k,'r) lin_partition = ('k key, 'r) Tjr_polymap.t

let rec key_compare k_cmp k1 k2 =
  match k1,k2 with 
  | Find _, Find _ -> failwith __LOC__  (* can only compare a Find to one of the other keys *)
  | _, Find _ -> -1*(key_compare k_cmp k2 k1)  (* Find must be the first argument *)
  | Less_than k1, Less_than k2 ->
    assert(k_cmp k1 k2 = 0);
    0
  | Less_than k1, Between (k2,k3) -> 
    assert (k_cmp k1 k2 <= 0);
    -1
  | Less_than k1, Greater_eq k2 -> 
    assert (k_cmp k1 k2 <= 0);
    -1
  | Between _, Less_than _ -> 
    -1*(key_compare k_cmp k2 k1)
  | Between(k2,k3), Between(k4,k5) -> 
    k_cmp k2 k4
  | Between(k2,k3), Greater_eq k4 -> 
    assert(k_cmp k3 k4 <= 0);
    -1
  | Greater_eq _, Less_than _ 
  | Greater_eq _, Between _ -> 
    -1*(key_compare k_cmp k2 k1)
  | Greater_eq k1, Greater_eq k2 -> 
    assert (k_cmp k1 k2 = 0);
    0
  | Find k1, Less_than k2 -> 
    if k_cmp k1 k2 < 0 then 0  (* k1 matches the interval < k2 *)
    else 1
  | Find k1, Between(k2,k3) -> 
    if k_cmp k1 k2 <0 then -1 else
    if k_cmp k2 k1 <=0 && k_cmp k1 k3 <0 then 0 else (
      assert(k_cmp k1 k3 >=0);
      1)
  | Find k1, Greater_eq k2 -> 
    if k_cmp k1 k2 >= 0 then 0 else
      -1

let examples = [Less_than 0; Between(0,1); Between(1,2); Greater_eq(2)]

module Ignore(_:sig end) = struct
let _ = List.map (key_compare Tjr_int.compare (Find(-1))) examples
let _ = List.map (key_compare Tjr_int.compare (Find(0))) examples
let _ = List.map (key_compare Tjr_int.compare (Find(1))) examples
let _ = List.map (key_compare Tjr_int.compare (Find(2))) examples
let _ = List.map (key_compare Tjr_int.compare (Find(3))) examples
end

let rec iter_opt f x =
  match f x with
  | Some x -> iter_opt f x
  | None -> x

(* we require at least one key and two r *)
let make_map k_cmp ks rs =
  assert(List.length ks >=1);
  assert(List.length ks +1 = List.length rs);
  let map = Tjr_polymap.empty (key_compare k_cmp) in
  let map = 
    (* deal with <k0 maps to r0 *)
    let k0,r0 = List.hd ks,List.hd rs in
    Tjr_polymap.add (Less_than k0) r0 map
  in
  (* now do the "betweens" *)
  iter_opt 
    (fun (map,ks,rs) -> 
       match (ks,rs) with 
       | k1::k2::_,r2::_ -> 
         Some(Tjr_polymap.add (Between(k1,k2)) r2 map, List.tl ks, List.tl rs)
       | _ -> None)
    (map,ks,List.tl rs)
  |> fun (map,[kn],[rn]) -> 
  Tjr_polymap.add (Greater_eq kn) rn map  [@@ocaml.warning "-8"]


(* test *)

let ex = make_map Tjr_int.compare [0;1;2;3] ["r0";"r1";"r2";"r3";"r4"]

let _ = 
  [-1;0;1;2;3;4] |> List.map (fun x ->
  Tjr_polymap.find (Find(x)) ex)



(* predecessor / successor relationship ----------------------------- *)

(* maintain a set of pairs (pred,succ)? or two maps? let's maintain a
   single map for the time being, for predecessor

  but the map already has (essentially) this structure; even so,
   perhaps better to separate out *)

(* this assumes there is some underlying order, and we just track a finite subset of this order *)
type ('k,'t) pred_succ_ops = {
  empty: unit -> 't;
  get_pred: 'k -> 't -> 'k option;
  get_succ: 'k -> 't -> 'k option;
  insert: 'k -> 't -> 't;
  remove: 'k -> 't -> 't;
  elements: 't -> 'k list;
}


(* implement the above using a set *)
open Tjr_poly_set 

type ('k,'t) set_ops = ('k,'t)Tjr_poly_set.set_ops

let make_pred_succ_ops set_ops =
  let empty () = set_ops.empty in
  let get_pred k t = set_ops.split k t |> fun (lower,_,_) -> set_ops.max_elt_opt lower in
  let get_succ k t = set_ops.split k t |> fun (_,_,higher) -> set_ops.min_elt_opt higher in
  let insert k t = set_ops.add k t in
  let remove k t = set_ops.remove k t in
  let elements t = set_ops.elements t in
  { empty; get_pred; get_succ; insert; remove; elements }

let _=  make_pred_succ_ops


(* example/test with ints ------------------------------------------- *)

module Ignore2(_:sig end) = struct

  let set_ops = Tjr_poly_set.make_set_ops (None:Tjr_set.Int_set.t option) (Tjr_int.compare)

  let pred_succ_ops = make_pred_succ_ops set_ops

  let _ = 
    let { empty; get_pred; get_succ; insert; remove; elements } = pred_succ_ops in
    let i2s i = match i with
      | None -> "-"
      | Some i -> string_of_int i
    in
    let print_state t = 
      elements t |> List.iter (fun x ->
          Printf.printf "Elt %d: pred=%s; succ=%s\n" 
            x 
            (get_pred x t |>i2s) 
            (get_succ x t |>i2s))
    in    
    iter_opt 
      (fun (t,ops) -> 
         match ops with 
         | [] -> None
         | op::ops -> 
           let t' = op t in
           Some(t',ops))
      (empty (),
       [insert 2;
        insert 4;
        insert 6;
        (fun t -> Printf.printf "%s\n" __LOC__; print_state t; t);
        remove 4;
        (fun t -> Printf.printf "%s\n" __LOC__; print_state t; t);
        remove 2;
        (fun t -> Printf.printf "%s\n" __LOC__; print_state t; t);
        insert 10;
        insert 11;
        remove 10;
        (fun t -> Printf.printf "%s\n" __LOC__; print_state t; t);
       ])
end
