open Shared_intf
open Blk_intf

(** The factory takes an arg of the form AN... and returns a result of the form RN...; add further args and results here *)

type arg = 
  | A1_string_4096
    (** blk is a string_4096 *)

  | A2_bytes_4096
    (** blk is a bytes_4096 *)

  | A3_ba_4096
    (** blk is a bigarray_4096 *)

type res = 
  | R1 of string blk_ops
  | R2 of bytes blk_ops
  | R3 of ba_buf blk_ops

let blk_sz = blk_sz_4096

let make = function
  | A1_string_4096 -> 
    R1 (Common_blk_ops.String_.make ~blk_sz)
  | A2_bytes_4096 -> 
    R2 (Common_blk_ops.Bytes_.make ~blk_sz)
  | A3_ba_4096 -> 
    R3 { blk_sz;
         of_string=(fun s ->
           assert(String.length s = Blk_sz.to_int blk_sz);        
           Bigstring.of_string s);
         to_string=(fun ba -> Bigstring.to_string ba);
         of_bytes=(fun bs ->
           assert(Bytes.length bs = Blk_sz.to_int blk_sz);
           Bigstring.of_bytes bs);
         to_bytes=(fun ba -> 
           Bigstring.to_bytes ba)
       }
