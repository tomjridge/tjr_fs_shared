(** A basic implementation of a block device. *)
open Tjr_monad.Monad_ops


(* blocks ----------------------------------------------------------- *)

(** Default implementation with block size of 4096 FIXME for testing *)
module Block_on_fd = Block_ops.String_block_ops.Make(struct let blk_sz=4096 end)

include Block_on_fd


include Blk_dev_ops_type.Generic_over_dev
(* so that when we open this module, we don't have to separately open
   Blk_dev_ops_type *)


(* type fd = Unix.file_descr *)

(* raw operations --------------------------------------------------- *)

module Internal = struct
  let read ~fd ~blk_sz ~blk_id = 
    ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
    let buf = Bytes.make blk_sz (Char.chr 0) in 
    let n = Unix.read fd buf 0 blk_sz in
    (* assert (n=blk_sz); we allow the file to expand automatically, so
       no reason to read any bytes since file could be empty *)
    (* test(fun _ -> assert(n=0 || n=blk_sz)); *)
    assert(n=0 || n=blk_sz);
    Bytes.to_string buf |> block_ops.of_string

  let write ~fd ~blk_sz ~blk_id ~blk = 
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

open Internal

(*
(** Default block size set arbitrarily at 4096 FIXME take care that
   this matches the underlying device, that aligned block writes via
   fd are atomic wrt the device etc *)
let default_blk_sz = 4096
*)

(** Construct a naive [blk_dev_ops] backed by a file. For testing. *)
let make_blk_dev_on_fd ~monad_ops = 
  (* let ( >>= ) = monad_ops.bind in *)
  let return = monad_ops.return in
  (* let blk_sz = default_blk_sz in *)
  let get_blk_sz fd = blk_sz in
  let read ~dev:fd ~blk_id = 
    return (read ~fd ~blk_sz ~blk_id)
  in
  let write ~dev:fd ~blk_id ~blk = 
    return (write ~fd ~blk_sz ~blk_id ~blk)
  in
  let generic_ops = {get_blk_sz; write; read} in
  fun k -> k 
      ~generic_ops 
      ~fixed_ops:(fun fd -> 
          Blk_dev_ops_type.fix_device ~dev:fd ~ops:generic_ops)


let _ = make_blk_dev_on_fd

