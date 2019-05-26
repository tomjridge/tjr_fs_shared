(** Simple in-memory block device. *)

open Blk_dev_ops_type

(** NOTE blk_sz is not checked - any size blk can be used *)
let make ~monad_ops ~blk_sz ~with_state = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in   
  let with_state = with_state.with_state in
  let write ~(blk_id:'blk_id) ~(blk:'blk) =
    with_state (fun ~state:s ~set_state ->
        set_state (Poly_map.With_pervasives_compare.add blk_id blk s) >>= fun _ -> 
        return ())
  in
  let read ~blk_id =
    (* NOTE assume never try to access an uninitialized blk *)
    with_state (fun ~state:s ~set_state ->
        return (Poly_map.With_pervasives_compare.find blk_id s))
  in
  { blk_sz; read; write }



let _ = make
