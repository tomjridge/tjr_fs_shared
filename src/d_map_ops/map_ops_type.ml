(* open Tjr_monad.Types *)

(** Map operations: find, insert, [insert_many] and delete. 

[insert_many] attempts to insert as many as possible in a
single operation, and returns the remainder, and so is typically
called in a loop (see {!Map_ops}).

The interfaces are heavily parameterized.  To understand the
interfaces, we need to introduce the following:

- Keys, represented by type variable ['k]
- Values, by type var ['v]
- Page/block references, ['r]; vars [blk_id]
- Phantom monad type, ['t]

The operations execute in the monad.

*)
type ('k,'v,'t) map_ops = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t)m;
  insert_many: 'k -> 'v -> ('k*'v) list -> (('k*'v)list,'t) m
}
