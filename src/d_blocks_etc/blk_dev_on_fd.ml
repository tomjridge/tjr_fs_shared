(** A basic implementation of a block device. *)
(* open Tjr_monad.Monad_ops *)
open Block_ops
open Blk_dev_ops_type


(* raw operations --------------------------------------------------- *)

module Internal = struct
  let read ~(block_ops:'blk Block_ops.block_ops) ~fd ~blk_id = 
    let blk_sz = block_ops.blk_sz |> Blk_sz.to_int in
    ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
    let buf = Bytes.make blk_sz (Char.chr 0) in 
    let n = Unix.read fd buf 0 blk_sz in
    (* assert (n=blk_sz); we allow the file to expand automatically, so
       no reason to read any bytes since file could be empty *)
    (* test(fun _ -> assert(n=0 || n=blk_sz)); *)
    assert(n=0 || n=blk_sz);
    Bytes.to_string buf |> block_ops.of_string

  let write ~(block_ops:'blk Block_ops.block_ops) ~fd ~blk_id ~blk = 
    let blk_sz = block_ops.blk_sz |> Blk_sz.to_int in
    let blk = block_ops.to_string blk in
    assert(String.length blk > 0);
    assert(blk_sz > 0);
    ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
    let buf = blk |> Bytes.of_string in
    (* Printf.printf "%d %d\n%!" (String.length blk) blk_sz; *)
    let n = Unix.single_write fd buf 0 blk_sz in
    (* test(fun _ -> assert (n=blk_sz)); *)
    assert (n=blk_sz);
    ()
end

(*
(** Default block size set arbitrarily at 4096 FIXME take care that
   this matches the underlying device, that aligned block writes via
   fd are atomic wrt the device etc *)
let default_blk_sz = 4096
*)

(** Construct a naive [blk_dev_ops] backed by a file. For testing. *)
let make_blk_dev_on_fd ~monad_ops ~(block_ops:'blk block_ops) ~fd = 
  (* let ( >>= ) = monad_ops.bind in *)
  let return = monad_ops.return in
  let blk_sz = block_ops.blk_sz in
  let read ~blk_id = 
    return (Internal.read ~block_ops ~fd ~blk_id)
  in
  let write ~blk_id ~blk = 
    return (Internal.write ~block_ops ~fd ~blk_id ~blk)
  in
  { blk_sz; read; write }

let _ = make_blk_dev_on_fd

