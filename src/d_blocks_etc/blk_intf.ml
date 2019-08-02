(** Blk-related interfaces 

The main choices:

- blk implementation (string, bytes, buf/bigarray etc)
- blk_id (presumably int)
- blk_dev (in mem, on file, on dev etc)
*)


module Blk_sz : sig 
  type blk_sz[@@deriving bin_io]
  val of_int: int -> blk_sz
  val to_int: blk_sz -> int
end = struct
  open Bin_prot.Std
  type blk_sz = int[@@deriving bin_io]  (* in bytes *)
  let of_int x = x
  let to_int x = x
end

type blk_sz = Blk_sz.blk_sz


module Blk_id : sig 
  type blk_id[@@deriving bin_io]
  val of_int: int -> blk_id
  val to_int: blk_id -> int
end = struct
  open Bin_prot.Std
  type blk_id = int[@@deriving bin_io]
  let of_int x = x
  let to_int x = x
end

type blk_id = Blk_id.blk_id


(** NOTE since blk_sz is expected to be fixed for a given blk type, we
   don't include as a parameter on the main interface methods 

FIXME get_blk_sz should be just blk_sz?
*)
type 'blk blk_ops = {
  blk_sz: blk_sz; 

  of_string: string -> 'blk;
  to_string: 'blk -> string;

  of_bytes: bytes -> 'blk;
  to_bytes: 'blk -> bytes;
}


(*
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
*)

(** This variant of blk_dev_ops has a fixed block device. *)
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


(** Wrap up common instances. Keep 'dev abstract since we may need to stage the construction. *)
type ('blk,'dev) blk_layer = {
  blk_ops: 'blk blk_ops;
  blk_dev_ops: 'dev
}
