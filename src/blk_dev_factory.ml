open Blk_intf
open Buf_ops

(** Common blk devs *)

(** Internal: blk_id is Blk_id_as_int *)
module Internal = struct
  type blk_id = Blk_id_as_int.blk_id
end
open Internal

(** The factory takes an arg of the form AN... and returns a result of the form RN...; add further args and results here *)

type arg = 
  | A1_string_4096_lwt_fd
    (** blk is a string_4096; monad is lwt; blk_dev on fd *)

  | A2_bytes_4096_lwt_fd
    (** blk is a bytes_4096; monad is lwt; blk_dev on fd *)

  | A3_bytes_4096_lwt_mem
    (** blk is a bytes_4096; monad is lwt; blk_dev in mem *)

  | A4_ba_4096_lwt_mem
    (** blk is a bigarray_4096; monad is lwt; blk_dev in mem *)
    
  | A5_blk_ba__lwt_fd
    (** blk is a ba_4096; monad is lwt; blk_dev on fd *)

  | A6_blk_ba__lwt of a6


and a6 = Filename of string | Fd of Lwt_unix.file_descr 
  


type 'blk r_3_4 = 
((blk_id,'blk) Tjr_map.With_pervasives_compare.map_with_pervasives_compare, lwt)
with_state
-> (blk_id, 'blk, lwt) blk_dev_ops

(* We get back a blk_dev and a function for closing the blk dev, by closing the underlying fd *)
module type R6 = sig
  val close_blk_dev : unit -> (unit, lwt) m
  val blk_dev : (blk_id, ba_buf, lwt) blk_dev_ops
end
(* FIXME make the other args return a similar result *)

type res = 
  | R1 of (Lwt_unix.file_descr -> (blk_id,string,lwt)blk_dev_ops)
  | R2 of (Lwt_unix.file_descr -> (blk_id,bytes,lwt)blk_dev_ops)
  | R3 of bytes r_3_4
  | R4 of ba_buf r_3_4
  | R5 of (Lwt_unix.file_descr -> (blk_id,ba_buf,lwt)blk_dev_ops)
  | R6 of ((module R6),lwt)m


module L = Tjr_monad.With_lwt

let rec make_6 (x:a6) = L.(
    x |> function
    | Filename filename -> (
        L.from_lwt (Lwt_unix.(openfile filename [O_CREAT;O_RDWR] Tjr_file.default_create_perm)) 
        >>= fun fd ->
        make_6 (Fd fd))
    | Fd fd -> 
      let blk_ops = Blk_factory.(make A3_ba_4096 |> fun (R3 x) -> x)[@@warning "-8"] in
      let module A = struct
        let close_blk_dev () = L.from_lwt(Lwt_unix.close fd) 
        let blk_dev = Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd
      end
      in
      return (module A : R6))

let _ = make_6

let make = function
  | A1_string_4096_lwt_fd -> 
    let blk_ops = Blk_factory.make_1 () in
    R1 (fun fd -> Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd)
  | A2_bytes_4096_lwt_fd -> 
    let blk_ops = Blk_factory.make_2 () in
    R2 (fun fd -> Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd)
  | A3_bytes_4096_lwt_mem -> 
    let blk_ops = Blk_factory.make_2 () in
    let f with_state = (
      Blk_dev_in_mem.make_blk_dev_in_mem
        ~monad_ops:lwt_monad_ops
        ~blk_sz:blk_sz_4096
        ~with_state)
    in
    R3 f
  | A4_ba_4096_lwt_mem ->
    (* let blk_ops = Common_blk_ops.Bytes_.make ~blk_sz:blk_sz_4096 in *)
    let f with_state = (
      Blk_dev_in_mem.make_blk_dev_in_mem
        ~monad_ops:lwt_monad_ops
        ~blk_sz:blk_sz_4096
        ~with_state)
    in
    R4 f
  | A5_blk_ba__lwt_fd ->
    let blk_ops = Blk_factory.(make A3_ba_4096 |> fun (R3 x) -> x)[@@warning "-8"] in
    R5 (fun fd -> Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd)
  | A6_blk_ba__lwt x -> R6 (make_6 x)
