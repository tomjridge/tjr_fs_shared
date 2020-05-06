(** A write-back cache; a wrapper around pqwy/lru.

This version avoids the extra ty var in v1.
*)



(** {2 Notes}

$(FIXME("odoc.css bold seems to render not bold; this will be fixed in
   new version of odoc"))

{%html: <style> b {font-weight:bold; } </style> %}

{b Note on pqwy/lru} pqwy/lru is here: {%html: <a href='https://github.com/pqwy/lru'>github</a> and <a
   href='https://pqwy.github.io/lru/doc/lru/Lru/index.html'>ocamldoc</a>
   %}

add promotes binding to the most recently used; trim needs to be
   invoked explicitly after add; find does not promote.

{b Additional operations} beyond pqwy/lru: we want to evict in
   batches; so we have a high capacity and a low capacity. We call
   low_cap just cap, and high_cap is low_cap+delta. We want a trim operation which trimes to low_cap.

We are also interested in whether entries are "clean" (can be evicted
   with no further action) or "dirty" (in which case, we need to flush
   to the lower level).

{b Note our find and insert do promote} (cf pqwy, which does not
   promote automatically).


*)

(** {2 API}

NOTE since we store a dirty flag (bool) with each value, some of the
operations take/return a ['v*bool] rather than a [v]

The operations:

- create: takes a capacity, and delta, the default number of lru-elts
  to remove on trim

- find, insert, delete; find and insert promote automatically; delete is used for B-tree freeing blks (we could trim instead)

- promote: to touch a key so that it has just been used; probably we don't use this (find and insert promote automatically)

- needs_trim: return true iff sz > cap+delta

- trim - pop lru clean and dirty elements until cap size reached; the returned list
  only contains the dirty entries 

- clean: get all the dirty entries, and return them, together
  with a fully clean, trimmed, cache; actually, since cleaning a large cache is very expensive, we just return an empty cache for now


$(TODO(""" At the moment, we use the pqwy functional implementation,
   but this is much slower than the imperative version which we should
   use in production; when we use a mutable version, we can also
   efficiently implement clean). """))

NOTE: for delete, we typically expect that any
existing v binding is not dirty, but this is not checked at runtime

$(ABBREV("wbc is short for write-back cache"))

*)


type ('k,'v,'t) wbc_ops = {
  size             : 't -> int;

  find             : 'k -> 't -> ('v * bool) option * 't;
  (** For find, if None is returned, the cache is guaranteed to be unaltered *)

  insert           : 'k -> 'v*bool -> 't -> 't;
  delete           : 'k -> 't -> 't;
  (* promote          : 'k -> 't -> 't; *)
  needs_trim       : 't -> bool;
  trim             : 't -> ('k*'v) list * 't;
  clean            : 't -> ('k*'v) list * 't;
  bindings         : 't -> ('k * ('v * bool)) list;
}

type ('k,'v,'t) wbc_o = < empty:'t; cap:int; delta:int; ops: ('k,'v,'t) wbc_ops >

type ('k,'v,'t) wbc_factory = < make_wbc: cap:int -> delta:int -> ('k,'v,'t)wbc_o >

module type K = Stdlib.Map.OrderedType

module type V = sig type t end

(** NOTE hidden doc [Pvt_make] *)

(**/**)

module Pvt_make(K:K)(V:V) = struct

  type k = K.t
  type v = V.t


  module V_x_dirty = struct
    type t = v * bool 
    let weight: t -> int = fun t -> 1
  end

  module Lru = Lru.F.Make(K)(V_x_dirty)

  let make_wbc ~cap ~delta = 
    let open (struct 

      let create () = Lru.empty cap

      let cap = cap

      let delta = delta

      let size t = Lru.size t

      let find k t = 
        Lru.find k t |> function
        | None -> None,t
        | Some vd -> 
          Lru.promote k t |> fun t' -> 
          Some vd,t'

      let insert k v t = 
        Lru.add k v t |> Lru.promote k

      let delete k t = Lru.remove k t

      let _promote k t = Lru.promote k t

      let needs_trim t = size t > cap+delta

      let trim t = 
        (t,size t,[]) |> iter_k (fun ~k:kont (t,sz,acc) -> 
            match sz <= cap with
            | true -> (acc,t)
            | false -> 
              Lru.pop_lru t |> function
              | None -> (acc,t)
              | Some(kv,t) -> 
                let (k,(v,dirty)) = kv in
                kont (t,sz-1,(if dirty then (k,v)::acc else acc)))

      (** For clean, we must construct a completely new lru; this is
          obviously very costly if the lru capacity is high; an
          alternative (which we use here) is just to return an empty
          lru FIXME is there a better way? *)
      let clean t = 
        Lru.fold_k (fun k (v,dirty) acc -> if dirty then (k,v)::acc else acc) [] t 
        |> fun dirties -> 
        dirties,Lru.empty cap

      let bindings t = Lru.to_list t

    end)
    in
    object 
      method empty=create() 
      method cap = cap
      method delta = delta
      method ops={ size; find; insert; delete;
                   (* promote; *)
                   needs_trim; trim; clean; bindings } 
    end

  let wbc_factory : (_,_,_) wbc_factory = object
    method make_wbc = make_wbc
  end

end

(**/**)

module Make(K:K)(V:V) : sig 
  type wbc
  val wbc_factory : (K.t,V.t,wbc) wbc_factory
end = struct 
  include Pvt_make(K)(V)
  type wbc = Lru.t
end

(** {[
module Make(K:K)(V:V) : sig 
  type wbc
  val wbc_factory : (K.t,V.t,wbc) wbc_factory
end
]} *)
