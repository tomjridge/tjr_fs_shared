open Tjr_monad.Types

include Map_ops_type



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
