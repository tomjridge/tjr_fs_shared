(* Prefer to use concrete implementations from shared_ctxt 

(** Implementations of blks; effectively blks are an abstract type
   which we can convert to/from bufs *)

open Buf_ops
open Blk_intf

let blk_ops = 
  object
    method ba_ba_4096 : (ba_buf,ba_buf)blk_ops = {
      blk_sz=blk_sz_4096;
      blk_to_buf=(fun blk -> blk);
      buf_to_blk=(fun buf -> 
          assert(Bigstring.length buf = 4096);
          buf)
    }
  end
*)
