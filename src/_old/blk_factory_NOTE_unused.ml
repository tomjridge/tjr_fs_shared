(*

(** Implementations of blks of fixed size; don't open; access via
   blks#string etc; no padding *)

open Blk_intf

module type S = sig val blk_sz:blk_sz end

(*
module Check(S:sig type t val blk_sz:blk_sz val length : t -> int end) = struct
  open S
  let blk_sz_i = Blk_sz.to_int blk_sz
  let check s = 
    assert(length s = blk_sz_i);
    ()
end
*)

(** Blks based on strings *)
module String_(S:S) = struct
  open S

  let blk_sz_i = Blk_sz.to_int blk_sz

  (* open (Check(struct include S type t = string let length = String.length end)) *)

  let of_string s =
    assert(String.length s = blk_sz_i);
    (* pad to length *)
    let s = s ^ (String.make (blk_sz_i - String.length s) '\x00') in
    s
  
  let to_string s = s 

  (** NOTE these involve substantial copying to/from string etc *)
  let of_bytes x = 
    assert(Bytes.length x = blk_sz_i);
    Bytes.to_string x

  let to_bytes = Bytes.of_string

  let of_ba x = 
    assert(Bigstring.length x = blk_sz_i);
    Bigstring.to_string x

  let to_ba = Bigstring.of_string

  let blk_ops = { blk_sz;of_string;to_string;of_bytes;to_bytes;of_ba;to_ba }

end


module Bytes_ = struct
  let make ~blk_sz = 
    let blk_sz' = Blk_sz.to_int blk_sz in
    let of_string s =
      assert (String.length s <= blk_sz');
      (* pad to length *)
      let s = s ^ (String.make (blk_sz' - String.length s) '\x00') in
      Bytes.of_string s
    in
    let to_string s = Bytes.to_string s in
    let of_bytes bs = 
      assert(Bytes.length bs = blk_sz');
      bs
    in
    let to_bytes blk = blk in
    (* let blk_sz = Blk_sz.of_int blk_sz in *)
    ({ blk_sz; of_string; to_string; of_bytes; to_bytes } : bytes blk_ops)  
end


(** The various make_n functions have types as follows (arg is a merlin-visible form of documentation *)

type arg = 
  | A1_string_4096
  | A2_bytes_4096
  | A3_ba_4096 (** *)
(** 
- A1 - blk is a string
- A2 - bytes
- A3 - bigarray *)

(*
type res = 
  | R1 of string blk_ops
  | R2 of bytes blk_ops
  | R3 of ba_buf blk_ops
*)

let blk_sz = blk_sz_4096

let make_1 () = Internal.String_.make ~blk_sz
let make_2 () = Internal.Bytes_.make ~blk_sz
let make_3 () = 
  { blk_sz;
    of_string=(fun s ->
      assert(String.length s <= Blk_sz.to_int blk_sz);        
      let buf = Bigstring.create (Blk_sz.to_int blk_sz) in
      Bigstring.blit_of_string s 0 buf 0 (String.length s);
      buf);
    to_string=(fun ba -> Bigstring.to_string ba);
    of_bytes=(fun bs ->
      assert(Bytes.length bs = Blk_sz.to_int blk_sz);
      Bigstring.of_bytes bs);
    to_bytes=(fun ba -> 
      Bigstring.to_bytes ba)
  }



(*
let make = function
  | A1_string_4096 -> R1 (make_1())
  | A2_bytes_4096 -> R2 (make_2())
  | A3_ba_4096 -> R3 (make_3())
*)
*)
