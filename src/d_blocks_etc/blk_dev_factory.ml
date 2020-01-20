open Blk_intf

(** The factory takes an arg of the form AN... and returns a result of the form RN...; add further args and results here *)

type arg = 
  | A1_string_4096_lwt_fd
    (** blk is a string_4096; monad is lwt; blk_dev on fd *)

  | A2_bytes_4096_lwt_fd
    (** blk is a bytes_4096; monad is lwt; blk_dev on fd *)

  | A3_bytes_4096_lwt_mem
    (** blk is a bytes_4096; monad is lwt; blk_dev in mem *)


type r3 = 
((Blk_id_as_int.blk_id,bytes) Tjr_map.With_pervasives_compare.map_with_pervasives_compare, lwt)
with_state
-> (Blk_id_as_int.blk_id, bytes, lwt) blk_dev_ops

type res = 
  | R1 of (Lwt_unix.file_descr -> (Blk_id_as_int.blk_id,string,lwt)blk_dev_ops)
  | R2 of (Lwt_unix.file_descr -> (Blk_id_as_int.blk_id,bytes,lwt)blk_dev_ops)
  | R3 of r3

let make = function
  | A1_string_4096_lwt_fd -> 
    let blk_ops = Common_blk_ops.String_.make ~blk_sz:blk_sz_4096 in
    R1 (fun fd -> Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd)
  | A2_bytes_4096_lwt_fd -> 
    let blk_ops = Common_blk_ops.Bytes_.make ~blk_sz:blk_sz_4096 in
    R2 (fun fd -> Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd)
  | A3_bytes_4096_lwt_mem -> 
    let blk_ops = Common_blk_ops.Bytes_.make ~blk_sz:blk_sz_4096 in
    let f with_state = (
      Blk_dev_in_mem.make_blk_dev_in_mem
        ~monad_ops:lwt_monad_ops
        ~blk_sz:blk_sz_4096
        ~with_state)
    in
    R3 f
