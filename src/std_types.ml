(** Common types we use in examples *)
open Buf_ops
(* open Bin_prot.Std *)

(** Standard types: t = lwt; blk=ba_buf; r=blk_id *)
type t = lwt
type blk = ba_buf
type blk_id = Blk_intf.Blk_id_as_int.blk_id[@@deriving bin_io]
type r = blk_id

