(** A concrete type for insert and delete operations.

Additional functionality in module {!Kv_op}. Prefer to open that module
 *)


(** An op is either insert or delete. These are the entries that get
   written to disk (find doesn't need to be written to disk since it
   doesn't change the on-disk state).

NOTE we may want to have variants of this with further operations *)
type ('k,'v) op = 
  | Insert of 'k * 'v 
  | Delete of 'k 
[@@deriving bin_io, yojson]

