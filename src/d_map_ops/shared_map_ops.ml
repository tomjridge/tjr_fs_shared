
(** Simple map operations: find, insert and delete (no insert_many). *)
module Fid_map_ops = struct

  type ('k,'v,'t) fid_map_ops = {
    find : 'k -> ('v option, 't) m;
    insert : 'k -> 'v -> (unit, 't) m;
    delete : 'k -> (unit, 't) m;
  }

end

module Map_ops_type = struct

  (** {2 Default map operations, with insert_many} *)

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

end

include Map_ops_type


module Internal_make_insert_many_insert_all = struct

  (** Utility: call [insert_many] in a loop. *)
  let insert_all ~monad_ops =
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    fun insert_many ->
      let rec loop k v kvs =
        insert_many k v kvs >>= (fun kvs' -> 
            match kvs' with
            | [] -> return ()
            | (k,v)::kvs -> loop k v kvs)
      in loop


  (** Utility: construct [insert_many] from [insert]. Obviously
      inefficient. Only for testing *)
  let make_insert_many ~monad_ops ~insert =
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in  
    fun (k:'v) (v:'v) (kvs:('k*'v)list) -> insert k v >>= fun () -> return kvs

end
