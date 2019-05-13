(** Block devices *)
open Blk_sz_type

module Internal_generic_over_dev = struct
  (** NOTE may need trim, FUA etc NOTE [dev] is the "type" of the
      device, not the device (ie state of device) itself (which is
      somehow accessed via the monad)

      FIXME this interface is deprecated; we prefer the non-dev-parameterized version
 *)
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
end

module Export = struct
  (** This variant has a fixed block device. *)
  type ('blk_id,'blk,'t) blk_dev_ops = {
    blk_sz: blk_sz; 
    write:
      blk_id:'blk_id -> 
      blk:'blk -> 
      (unit,'t) m;
    read:
      blk_id:'blk_id -> 
      ('blk,'t) m;
  }
end

include Export
