(** Operations on blocks *)

open Blk_sz



(** NOTE since blk_sz is expected to be fixed for a given blk type, we
   don't include as a parameter on the main interface methods 

FIXME get_blk_sz should be just blk_sz?
*)
type 'blk block_ops = {
  blk_sz: blk_sz; 

  of_string: string -> 'blk;
  to_string: 'blk -> string;

  of_bytes: bytes -> 'blk;
  to_bytes: 'blk -> bytes;
}
  
