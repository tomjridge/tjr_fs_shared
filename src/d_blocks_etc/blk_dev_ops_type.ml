(** Block devices *)
open Tjr_monad.Monad_ops

type ('blk_id,'blk,'dev,'t) blk_dev_ops = {
  write:
    dev:'dev -> 
    blk_id:'blk_id -> 
    blk:'blk -> 
    (unit,'t) m;
  read:
    dev:'dev -> 
    blk_id:'blk_id -> 
    ('blk,'t) m;
}

