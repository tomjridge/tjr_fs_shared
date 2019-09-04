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
    
  (* a block size of 4096 *)
  val blk_sz_4096 : blk_sz
end = struct
  open Bin_prot.Std
  type blk_sz = int[@@deriving bin_io]  (* in bytes *)
  let of_int x = x
  let to_int x = x
  let blk_sz_4096 = 4096
end

type blk_sz = Blk_sz.blk_sz

let blk_sz_4096 = Blk_sz.blk_sz_4096

(** This is a common instance of blk_id; we don't open it by default
   because we want most code to be independent of the exact repn. of
   blk_id NOTE do not open this module *)
module Blk_id_as_int : sig 
  type blk_id[@@deriving bin_io]
  val of_int: int -> blk_id
  val to_int: blk_id -> int
  val incr: blk_id -> blk_id
end = struct
  open Bin_prot.Std
  type blk_id = int[@@deriving bin_io]
  let of_int x = x
  let to_int x = x
  let incr x = x+1
end

(* type blk_id = Blk_id.blk_id *)


(** 

NOTE Conversion to a blk is expected (and the common instances do,
indeed!) to pad if the string/bytes are not long enough.

NOTE since blk_sz is expected to be fixed for a given blk type, we
   don't include as a parameter on the main interface methods 

*)
type 'blk blk_ops = {
  blk_sz: blk_sz; 

  of_string: string -> 'blk;
  to_string: 'blk -> string;

  of_bytes: bytes -> 'blk;
  to_bytes: 'blk -> bytes;
}

(** This variant of blk_dev_ops has a fixed block device. *)
type ('blk_id,'blk,'t) blk_dev_ops = {
  blk_sz: blk_sz; 
  write: blk_id:'blk_id -> blk:'blk -> (unit,'t) m;
  read: blk_id:'blk_id -> ('blk,'t) m;
  write_many: ('blk_id*'blk)list -> (unit,'t) m  (* FIXME may want to make this a seq? *)
}

(* This is used for talks, to avoid explaining labelled args *)
module Internal_unlabelled_blk_dev_ops = struct

  type ('blk_id,'blk,'t) blk_dev_ops = {
    blk_sz: blk_sz; 
    write: 'blk_id -> 'blk -> (unit,'t) m;
    read: 'blk_id -> ('blk,'t) m;
  }

end


(** Wrap up common instances. Keep 'dev abstract since we may need to stage the construction. *)
type ('blk,'dev) blk_layer = {
  blk_ops: 'blk blk_ops;
  blk_dev_ops: 'dev
}
