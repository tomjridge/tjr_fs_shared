(** A collection of common block instances. FIXME use a GADT or similar to cut down the number of variations *)

open Blk_intf
open Blk_layer

(* A block layer using string blocks, and blk dev on fd *)
let blk_layer_unix_string_fd ~blk_sz = 
  let blk_ops = Common_blk_ops.String_.make ~blk_sz in
  {
    blk_ops;
    blk_dev_ops = (fun ~monad_ops ~fd -> Blk_dev_on_fd.make_with_unix ~monad_ops ~blk_ops ~fd)
  }

(* A block layer using string blocks, and blk dev on fd *)
let blk_layer_lwt_string_fd ~blk_sz = 
  let blk_ops = Common_blk_ops.String_.make ~blk_sz in
  {
    blk_ops;
    blk_dev_ops = (fun ~fd -> Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd)
  }


(* A block layer using string blocks, and blk dev on fd *)
let blk_layer_string_mem ~blk_sz = 
  let blk_ops = Common_blk_ops.String_.make ~blk_sz in
  {
    blk_ops;
    blk_dev_ops = (fun ~monad_ops ~with_state -> Blk_dev_in_mem.make_blk_dev_in_mem ~monad_ops ~blk_sz ~with_state)
  }
