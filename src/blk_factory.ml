(* open Shared_intf *)
(* open Buf_ops *)
open Blk_intf

module Internal = struct
  (** Some simple implementations of blocks with different underlying types *)

  (* FIXME merge into blk_factory *)

  open Blk_intf

  (* FIXME a block could be a sum type, either string, bytes, or buffer;
     then conversions happen lazily on demand *)

  module String_ = struct
    (** NOTE these involve substantial copying to/from string etc *)
    let make ~blk_sz = 
      let blk_sz' = Blk_sz.to_int blk_sz in
      (* let get_blk_sz () = blk_sz in *)
      let of_string s =
        assert (String.length s <= blk_sz');
        (* pad to length *)
        let s = s ^ (String.make (blk_sz' - String.length s) '\x00') in
        s
      in
      let to_string s = s in
      let of_bytes bs = of_string (Bytes.to_string bs) in
      let to_bytes b = Bytes.of_string b in
      (* let blk_sz = Blk_sz.of_int blk_sz in *)
      { blk_sz; of_string; to_string; of_bytes; to_bytes }

    (* FIXME add further implementations here *)
  end

  (** use a string; size is 4096 *)
  let string_blk_ops = String_.make ~blk_sz:Blk_sz.blk_sz_4096


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

  (** use bytes; size is 4096 *)
  let bytes_blk_ops = Bytes_.make ~blk_sz:Blk_sz.blk_sz_4096
end
(* open Internal *)

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
      assert(String.length s = Blk_sz.to_int blk_sz);        
      Bigstring.of_string s);
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
