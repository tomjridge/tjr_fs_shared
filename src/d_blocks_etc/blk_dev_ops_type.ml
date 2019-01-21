(** Block devices *)
open Tjr_monad.Monad_ops
open Blk_sz_type

(** NOTE may need trim, FUA etc NOTE [dev] is the "type" of the
   device, not the device (ie state of device) itself (which is
   somehow accessed via the monad) *)
type ('blk_id,'blk,'dev,'t) blk_dev_ops = {
  get_blk_sz: 'dev -> blk_sz; 
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

