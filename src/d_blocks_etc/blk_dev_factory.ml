open Blk_intf

(** The factory takes an arg of the form AN... and returns a result of the form RN...; add further args and results here *)

type arg = 
  | A1_string_4096_lwt
    (** blk is a string_4096; monad is lwt *)

  | A2_bytes_4096_lwt

type res = 
  | R1 of (Lwt_unix.file_descr -> (Blk_id_as_int.blk_id,string,lwt)blk_dev_ops)
  | R2 of (Lwt_unix.file_descr -> (Blk_id_as_int.blk_id,bytes,lwt)blk_dev_ops)

let make = function
  | A1_string_4096_lwt -> 
    let blk_ops = Common_blk_ops.String_.make ~blk_sz:blk_sz_4096 in
    R1 (fun fd -> Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd)
  | A2_bytes_4096_lwt -> 
    let blk_ops = Common_blk_ops.Bytes_.make ~blk_sz:blk_sz_4096 in
    R2 (fun fd -> Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd)
