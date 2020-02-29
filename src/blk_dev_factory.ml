(** Common blk devs; don't open *)

[%%import "config.ml"]

open Blk_intf
open Buf_ops

(** don't open this module *)
type blk_id = Blk_id_as_int.blk_id
type ('blk_id,'blk,'t)blk_dev_ops = ('blk_id,'blk,'t)Blk_intf.blk_dev_ops
type ('k,'v)stdmap = ('k,'v) Tjr_map.With_stdcmp.stdmap

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

  (* | A6_blk_ba__lwt of a6 *)

  | A7_blk_ba__lwt of Lwt_unix.file_descr
  | A8_blk_ba__lwt of string

(* and a6 = Filename of string | Fd of Lwt_unix.file_descr  *)

(*  
open Tjr_map
open Tjr_map.With_stdcmp

type 'blk r_3_4 = 
  ((blk_id,'blk,(blk_id,'blk)stdmap)map_ops, lwt) with_state
  -> (blk_id, 'blk, lwt) blk_dev_ops
*)

(* We get back a blk_dev and a function for closing the blk dev, by
   closing the underlying fd *)
module type R6 = sig
  val fd : Lwt_unix.file_descr
  val close_blk_dev : unit -> (unit, lwt) m
  val blk_dev_ops : (blk_id, ba_buf, lwt) blk_dev_ops
end
(* FIXME make the other args return a similar result *)

(*
type res = 
  | R1 of (Lwt_unix.file_descr -> (blk_id,string,lwt)blk_dev_ops)
  | R2 of (Lwt_unix.file_descr -> (blk_id,bytes,lwt)blk_dev_ops)
  | R3 of bytes r_3_4
  | R4 of ba_buf r_3_4
  | R5 of (Lwt_unix.file_descr -> (blk_id,ba_buf,lwt)blk_dev_ops)
  | R6 of ((module R6),lwt)m
*)

type lwt = Tjr_monad.lwt


(**/**)
module L = Tjr_monad.With_lwt 

[%%if PROFILE_BLK_DEV]
let profile_blk_dev = true
[%%else]
let profile_blk_dev = false
[%%endif]

let 
  [r1; w1; w2 ] =
  ["r1" ; "w1"; "w2" ]
  |> List.map Tjr_profile.intern[@@warning "-8"]

let add_profiling blk_dev_ops = 
  let open Tjr_monad.With_lwt in
  let prf = Tjr_profile.make_profiler 
      ~print_header:"blk profiler" 
      ~print_at_exit:true () 
  in
  let { blk_sz; read; write; write_many } = blk_dev_ops in
  let read ~blk_id = 
    prf.mark r1;
    read ~blk_id >>= fun b ->
    prf.mark (-1*r1);
    return b
  in
  let write ~blk_id ~blk = 
    prf.mark w1;
    write ~blk_id ~blk >>= fun () ->
    prf.mark (-1*w1);
    return ()
  in
  let write_many ws = 
    prf.mark w2;
    write_many ws >>= fun () ->
    prf.mark (-1*w2);
    return ()
  in
  { blk_sz; read;write;write_many }
(**/**)

let make_1 fd : (blk_id,string,lwt)blk_dev_ops= 
  let blk_ops = Blk_factory.make_1 () in
  Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd

let make_2 fd : (blk_id,bytes,lwt)blk_dev_ops = 
  let blk_ops = Blk_factory.make_2 () in
  Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd

let make_3 () : (('a, 'b) stdmap, lwt) Tjr_monad.with_state -> ('a, 'b, lwt) blk_dev_ops= 
  let blk_ops = Blk_factory.make_2 () in
  let f with_state = (
    Blk_dev_in_mem.make_blk_dev_in_mem
      ~monad_ops:lwt_monad_ops
      ~blk_sz:blk_sz_4096
      ~with_state)
  in
  f

let make_4 : (('a,'b)stdmap,lwt)with_state -> ('a,'b,lwt)blk_dev_ops = 
  (* let blk_ops = Common_blk_ops.Bytes_.make ~blk_sz:blk_sz_4096 in *)
  let f with_state = (
    Blk_dev_in_mem.make_blk_dev_in_mem
      ~monad_ops:lwt_monad_ops
      ~blk_sz:blk_sz_4096
      ~with_state)
  in
  f

let make_5 fd : (blk_id,Bigstring.t,lwt)blk_dev_ops = 
  let blk_ops = Blk_factory.(make_3 ()) in
  Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd

(*
let rec make_6 (x:a6) : ((module R6),lwt)m = L.(
    x |> function
    | Filename filename -> (
        L.from_lwt (Lwt_unix.(openfile filename [O_CREAT;O_RDWR] Tjr_file.default_create_perm)) 
        >>= fun fd ->
        make_6 (Fd fd))
    | Fd fd -> 
      let blk_ops = Blk_factory.(make_3 ()) in
      let module A = struct
        let fd = fd 
        let close_blk_dev () = L.from_lwt(Lwt_unix.close fd) 
        let blk_dev = Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd
        let blk_dev_ops = profile_blk_dev |> function
          | true -> add_profiling blk_dev
          | false -> blk_dev
      end
      in
      return (module A : R6))

let _ = make_6
*)


let make_7 fd = 
  let blk_ops = Blk_factory.(make_3 ()) in
  let module A = struct
    let fd = fd 
    let close_blk_dev () = L.from_lwt(Lwt_unix.close fd) 
    let blk_dev = Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd
    let blk_dev_ops = profile_blk_dev |> function
      | true -> add_profiling blk_dev
      | false -> blk_dev
  end
  in
  (module A : R6)

let make_8 fn = L.(
    from_lwt (Lwt_unix.(openfile fn [O_CREAT;O_RDWR] Tjr_file.default_create_perm)) 
    >>= fun fd -> return (make_7 fd))

(*
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
*)

