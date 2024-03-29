(** Simple in-memory block device. NOTE don't access this directly -
   use blk_devs from blk_dev_factory. *)

open Blk_intf

(** NOTE blk_sz is not checked - any size blk can be used *)
let make_blk_dev_in_mem ~monad_ops ~blk_sz ~with_state = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in   
  let with_state = with_state.with_state in
  let write ~(blk_id:'blk_id) ~(blk:'blk) =
    with_state (fun ~state:s ~set_state ->
        set_state (Tjr_map.With_stdcmp.add blk_id blk s) >>= fun _ -> 
        return ())
  in
  let read ~blk_id =
    (* NOTE assume never try to access an uninitialized blk *)
    with_state (fun ~state:s ~set_state ->
        return (Tjr_map.With_stdcmp.find blk_id s))
  in
  let write_many writes = 
    writes |> Util.iter_k (fun ~k ws -> match ws with
        | [] -> return ()
        | (blk_id,blk)::ws -> 
          write ~blk_id ~blk >>= fun () -> 
          k ws)
  in
  { blk_sz; read; write; write_many }



let _ = make_blk_dev_in_mem
