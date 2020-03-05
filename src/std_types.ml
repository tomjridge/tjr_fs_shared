(** Common types we use in examples *)
open Buf_ops
(* open Bin_prot.Std *)

(** Standard types: t = lwt; blk=ba_buf; r=blk_id *)
type t = lwt
type blk = ba_buf
type blk_id = Blk_intf.Blk_id_as_int.blk_id[@@deriving bin_io]
type r = blk_id[@@deriving bin_io]
let r_cmp : r -> r -> int = Stdlib.compare
type buf = ba_buf

let monad_ops = lwt_monad_ops

let blk_ops = Blk_factory.make_3()

let blk_sz = Blk_intf.blk_sz_4096
    

module B = Blk_intf.Blk_id_as_int

let r_size = 9 (* max size of r=blk_id when marshalled *)

open Blk_intf
type nonrec std_blk_dev_ops = (blk_id,blk,t)blk_dev_ops
type nonrec std_blk_allocator_ops = (r,t)blk_allocator_ops
