(** Disk block operations: read, write *)

(* TODO and sync *)

(* open Base_types *)
open Tjr_monad.Types
open Block

(* block device ---------------------------------------- *)

(* FIXME move to base types *)
module Block_device_type = struct
  type 't block_device = {
    blk_sz:blk_sz;  (* FIXME maybe blk_sz: 'dev -> int *)
    read:blk_id -> (blk,'t) m;
    write:blk_id -> blk -> (unit,'t) m
  }
end
include Block_device_type

