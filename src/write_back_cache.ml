(** A wrapper around pqwy/lru.

At the moment, we use the pqwy functional implementation, but this is
   much slower than the imperative version (which we should use in
   production TODO).

*)

(* FIXME FIXME use tjr_lib_core.tjr_lru *)

open Shared_intf

(** 
NOTE since we store a dirty flag (bool) with each value, some of the operations take/return a 'v*bool rather than a v

The operations:

- create: takes a capacity, and delta, the default number of lru-elts
  to remove on trim (typically 1)
- insert
- delete
- find
- trim_1, which returns the trimmed value and an indication of whether it was dirty
- trim delta - try to pop delta lru-elts; the delta arg is optional
  (it uses the value provided on create by default); the returned list
  only contains the dirty entries (so we don't handle clean entries)
- trim_all - trim all elts; return filtered and an empty wbc
- promote: to touch a key so that it has just been used (find does not
  automatically promote... perhaps it should?)

NOTE: for insert and delete operations, we typically expect that any
existing v binding is not dirty, but this is not checked at runtime

*)


type ('k,'v,'a,'t) write_back_cache_ops = {
  find             : 'k -> 't -> ('v * bool) option;
  insert           : 'k -> 'v*bool -> 't -> 't;
  delete           : 'k -> 't -> 't;
  promote          : 'k -> 't -> 't;
  trim_1           : 't -> (('k * ('v * bool))*'t) option;
  trim             : ?filter_map:('k * ('v * bool) -> 'a option) -> ?delta:int -> 't -> 'a list * 't;
  trim_if_over_cap : ?filter_map:('k * ('v * bool) -> 'a option) -> 't -> ('a list * 't) option; (* could be 'b *)
  trim_all         : ?filter_map:('k * ('v * bool) -> 'a option) -> 't -> ('a list * 't); 
  size             : 't -> int
}

module Make_write_back_cache(K:Stdlib.Map.OrderedType)(V:sig type t end) = struct

  module Internal = struct

    type k = K.t
    type v = V.t

    module V_x_dirty = struct
      type t = v * bool
      let weight: t -> int = fun t -> 1
    end

    module Lru = Lru.F.Make(K)(V_x_dirty)

    let find k t = Lru.find k t |> function 
      | None -> None 
      | Some v -> Some v

    let insert k v t =       
      Lru.add k v t

    let delete k t = Lru.remove k t

    let trim_1 t = Lru.pop_lru t

    let size t = Lru.size t

    let create ~cap = Lru.empty cap

    let promote k t = Lru.promote k t
  end
  open Internal

  (** NOTE by default trim only returns the dirty elements *)
  let make_write_back_cache ~cap ~delta = 
    let delta0 = delta in
    let filter_map = fun (k,(v,dirty)) -> if dirty then Some(k,v) else None in
    let trim ?(filter_map=filter_map) ?(delta=delta0) t : ('a list * Lru.t) =
      (t,[],0) |> iter_k (fun ~k (t,acc,count) -> 
          match count >= delta with
          | true -> (acc,t)
          | false -> (
              Lru.pop_lru t |> function
              | None -> (acc,t)
              | Some(kv,t) -> (
                  let acc' = match filter_map kv with None -> acc | Some a -> a::acc in
                  k (t,acc',count+1)))
        )
    in
    let trim_if_over_cap ?(filter_map=filter_map) t = 
      match size t > cap with
      | false -> None
      | true -> Some(trim ~filter_map t)
    in
    let trim_all ?(filter_map=filter_map) t =
      trim ~filter_map ~delta:cap t
    in
    let create = create ~cap in
    { initial_state=create; ops= { find; insert; delete; promote; trim_1; trim; trim_if_over_cap; trim_all; size }}

  let _ 
: cap:int ->
delta:int ->
(Internal.Lru.t, (k, v, k * v, Internal.Lru.t) write_back_cache_ops)
initial_state_and_ops
= make_write_back_cache

end
