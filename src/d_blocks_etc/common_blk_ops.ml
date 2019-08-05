(** Some simple implementations of blocks with different underlying types *)

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

(*
  module type T = sig
    val blk_sz : int
    type blk
    val blk_ops : blk blk_ops
  end


  module Make(S:sig val blk_sz: int end) : T = struct
    open S
    type blk = string
    let blk_sz = blk_sz
    let blk_ops = make_string_blk_ops ~blk_sz
  end
*)

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

(** use bytes; size if 4096 *)
let bytes_blk_ops = Bytes_.make ~blk_sz:Blk_sz.blk_sz_4096


module Buf_ = struct end (* FIXME *)



