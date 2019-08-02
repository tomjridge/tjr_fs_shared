(** A collection of common block instances. *)

open Blk_intf

(* A block layer using string blocks, and blk dev on fd *)
let blk_layer_string_fd ~blk_sz = 
  let blk_ops = Common_blk_ops.String_.make ~blk_sz in
  {
    blk_ops;
    blk_dev_ops = (fun ~monad_ops ~fd -> Blk_dev_on_fd.make_blk_dev_on_fd ~monad_ops ~blk_ops ~fd)
  }

(* A block layer using string blocks, and blk dev on fd *)
let blk_layer_string_mem ~blk_sz = 
  let blk_ops = Common_blk_ops.String_.make ~blk_sz in
  {
    blk_ops;
    blk_dev_ops = (fun ~monad_ops ~with_state -> Blk_dev_in_mem.make_blk_dev_in_mem ~monad_ops ~blk_sz:(Blk_sz.of_int blk_sz) ~with_state)
  }
