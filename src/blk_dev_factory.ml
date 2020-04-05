(** Common blk devs; don't open *)

[%%import "config.ml"]

open Blk_intf
open Buf_ops

let default_create_perm = Tjr_file.default_create_perm


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

module Pvt = struct
  [%%if PROFILE_BLK_DEV]
  let profile_blk_dev = true
  let _ : unit = Printf.printf "Blk_dev_factory: profiling enabled\n%!"
  [%%else]
  let profile_blk_dev = false
  [%%endif]

  [%%if DEBUG_BLK_DEV]
  let debug_blk_dev = true
  let _ : unit = Printf.printf "Blk_dev_factory: debug enabled\n%!"
  [%%else]
  let debug_blk_dev = false
  [%%endif]



  let 
    [r1; w1; w2 ] =
    ["r1" ; "w1"; "w2" ]
    |> List.map Tjr_profile.intern[@@warning "-8"]

  let add_profiling blk_dev_ops = 
    let open Tjr_monad.With_lwt in
    let prf = Tjr_profile.make_profiler 
        ~print_header:(Printf.sprintf "blk profiler %s" __LOC__)
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


  let add_debug_to_blk_dev_ops (blk_dev_ops:(blk_id,_,_)blk_dev_ops) = 
    let module B = Blk_id_as_int in
    let read ~blk_id =
      Printf.printf "blk_dev_ops %s: read %d\n%!" __LOC__ (B.to_int blk_id);
      blk_dev_ops.read ~blk_id
    in
    let write ~blk_id ~blk = 
      Printf.printf "blk_dev_ops %s: write %d\n%!" __LOC__ (B.to_int blk_id);
      blk_dev_ops.write ~blk_id ~blk
    in
    { blk_dev_ops with read; write }

  let _ = add_debug_to_blk_dev_ops                           

  let maybe_debug = fun x -> 
    if debug_blk_dev then x |> add_debug_to_blk_dev_ops else x
end
open Pvt
(**/**)

let make_1 fd : (blk_id,string,lwt)blk_dev_ops= 
  let blk_ops = Blk_factory.make_1 () in
  Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd |> maybe_debug

let make_2 fd : (blk_id,bytes,lwt)blk_dev_ops = 
  let blk_ops = Blk_factory.make_2 () in
  Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd |> maybe_debug

let make_3 () : (('a, 'b) stdmap, lwt) Tjr_monad.with_state -> ('a, 'b, lwt) blk_dev_ops= 
  let blk_ops = Blk_factory.make_2 () in
  let f with_state = (
    Blk_dev_in_mem.make_blk_dev_in_mem
      ~monad_ops:lwt_monad_ops
      ~blk_sz:blk_sz_4096
      ~with_state
    |> maybe_debug)
  in
  f

let make_4 : (('a,'b)stdmap,lwt)with_state -> ('a,'b,lwt)blk_dev_ops = 
  (* let blk_ops = Common_blk_ops.Bytes_.make ~blk_sz:blk_sz_4096 in *)
  let f with_state = (
    Blk_dev_in_mem.make_blk_dev_in_mem
      ~monad_ops:lwt_monad_ops
      ~blk_sz:blk_sz_4096
      ~with_state
    |> maybe_debug)
  in
  f

let make_5 fd : (blk_id,Bigstring.t,lwt)blk_dev_ops = 
  let blk_ops = Blk_factory.(make_3 ()) in
  Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd |> maybe_debug

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
    let blk_dev_ops = blk_dev_ops |> maybe_debug
  end
  in
  (module A : R6)

let make_8 fn = L.(
    from_lwt (Lwt_unix.(openfile fn [O_CREAT;O_RDWR] Tjr_file.default_create_perm)) 
    >>= fun fd -> return (make_7 fd))


let make_9 fn = L.(
    let blk_ops = Blk_factory.(make_3 ()) in
    from_lwt Lwt_unix.(openfile fn [O_CREAT;O_RDWR] default_create_perm) >>= fun fd -> 
    let blk_dev = Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd in
    let blk_dev_ops = profile_blk_dev |> function
      | true -> add_profiling blk_dev
      | false -> blk_dev in
    let blk_dev_ops = blk_dev_ops |> maybe_debug in
    return (object
      method blk_dev_ops=blk_dev_ops
      method fd=fd
    end))
         
let _ = make_9

        
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

