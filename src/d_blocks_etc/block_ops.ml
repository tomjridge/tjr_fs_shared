(** Some simple implementations of blocks with different underlying types *)

include Block_ops_type

(* FIXME a block could be a sum type, either string, bytes, or buffer;
   then conversions happen lazily on demand *)

(** NOTE these involve substantial copying to/from string etc *)
let make_string_block_ops ~blk_sz = 
  let get_blk_sz () = blk_sz in
  let of_string s =
    assert (String.length s <= blk_sz);
    (* pad to length *)
    let s = s ^ (String.make (blk_sz - String.length s) '\x00') in
    s
  in
  let to_string s = s in
  let of_bytes bs = of_string (Bytes.to_string bs) in
  let to_bytes b = Bytes.of_string b in
  { get_blk_sz; of_string; to_string; of_bytes; to_bytes }


(* FIXME add further implementations here *)
