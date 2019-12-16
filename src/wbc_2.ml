(** A wrapper around pqwy/lru.

At the moment, we use the pqwy functional implementation, but this is
   much slower than the imperative version (which we should use in
   production TODO).

NOTE this is a simplification of the original write_back_cache.ml

*)

(* open Shared_intf *)

(** 
NOTE since we store a dirty flag (bool) with each value, some of the operations take/return a 'v*bool rather than a v

The operations: as per Tjr_lru, but record an additional "dirty" flag with each value.

NOTE: for insert and delete operations, we typically expect that any
existing v binding is not dirty, but this is not checked at runtime

*)


module Wbc_ops = struct

  type ('k,'v,'t) wbc_ops = {
    empty    : int -> 't;
    is_empty : 't -> bool;
    capacity : 't -> int;
    size     : 't -> int;  (* may be over cap *)
    find     : 'k -> 't -> ('v * bool) option;
    insert   : 'k -> 'v * bool -> 't -> 't;
    delete   : 'k -> 't -> 't;
    promote  : 'k -> 't -> 't;
    trim_1   : 't -> (('k*('v*bool)) * 't)option;
    trim     : 't -> ('k*('v*bool))list * 't; (* remove LRU entries till below cap *)
  }
end

type ('k,'v,'t) wbc_ops = ('k,'v,'t) Wbc_ops.wbc_ops


module Make(K:Stdlib.Map.OrderedType)(V:sig type t end) = struct

  module V' = struct type t = V.t * bool end

  module Lru = Tjr_lru.Make(K)(V')

  let _lru_ops : (K.t,V.t*bool,Lru.Internal.t) lru_ops = Lru.lru_ops

  include Wbc_ops  (* so the make is self-contained *)

  type t = Lru.Internal.t

  let wbc_ops : (K.t,V.t,t) wbc_ops = 
    let Tjr_lru.Lru_ops.{ empty; is_empty; capacity; size; find; insert; delete; promote; trim_1; trim } = Lru.lru_ops in
    Wbc_ops.{ empty; is_empty; capacity; size; find; insert; delete; promote; trim_1; trim }

end
