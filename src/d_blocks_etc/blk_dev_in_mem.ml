(** Simple in-memory block device. *)

open Tjr_monad.Types
open Blk_dev_ops_type

module Internal = struct
  type blk_id = int
  type blk = string
  type dev = blk Tjr_map.Map_int.t
  type dev_type
end
open Internal

open Tjr_map

let make ~monad_ops ~blk_sz ~with_state = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in   
  let get_blk_sz dev = blk_sz in
  let write ~(dev:dev_type) ~blk_id ~blk =
    with_state (fun ~state:s ~set_state ->
        set_state (Map_int.add blk_id blk s) >>= fun _ -> 
      return ())
  in
  let read ~(dev:dev_type) ~blk_id =
    (* NOTE assume never try to access an uninitialized blk *)
    with_state (fun ~state:s ~set_state ->
        return (Map_int.find blk_id s))
  in
  { get_blk_sz; read; write }
