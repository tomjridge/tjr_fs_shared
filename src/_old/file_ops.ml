(** To make the code more uniform, we assume various "normal" file
   operations (ie over the local filesystem) are available in the
   monad. Of course, these operations are imperative since they use a
   real underlying filesystem. *)

open Tjr_file
open Tjr_file.Filenames

(** This is minimal, so we can support blk-dev-on-fd initialization
   and finalization. pread and pwrite always use blk-sized amounts of
   data. read_blk and write_blk take a blk_index *)
type ('fd,'blk,'t) file_ops = {
  read_file            : fn -> (string,'t)m;
  write_string_to_file : fn:fn -> string -> (unit,'t)m;
  stat                 : fn -> (fds_t option,'t)m;
  open_                : fn:fn -> create:bool -> init:bool -> ('fd,'t)m;
  close                : 'fd -> (unit,'t)m;
  blk_sz               : int;
  read_blk             : 'fd -> int -> ('blk,'t)m;
  write_blk            : 'fd -> int -> 'blk -> (unit,'t)m;
}


module Lwt_file_ops__bytes : sig
  type fd = Lwt_unix.file_descr
  type blk = bytes
  val lwt_file_ops : (fd,blk,lwt) file_ops
end = struct 
  type fd = Lwt_unix.file_descr
      
  (* let monad_ops = Tjr_monad.With_lwt.lwt_monad_ops *)

  open Tjr_monad.With_lwt

  let read_file fn = 
    Tjr_file.read_file fn |> return

  let write_string_to_file ~fn s =
    Tjr_file.write_string_to_file ~fn s |> return

  let stat fn = Tjr_file.stat fn |> return

  let open_ ~fn ~create ~init = Lwt_unix.(
      let flgs = [O_RDWR] @ (if create then [O_CREAT] else []) in
      from_lwt(openfile fn flgs 0o640) >>= fun fd -> 
      (if init then from_lwt(ftruncate fd 0) else return ()) >>= fun _ -> 
      return fd)

  let close fd = from_lwt(Lwt_unix.close fd)

  let blk_sz = 4096  (* perhaps this should be configurable? *)

  type blk = bytes

  let read_blk fd blk_index =
    let buf = Bytes.create blk_sz in
    from_lwt (Lwt_unix.lseek fd (blk_sz*blk_index) SEEK_SET) >>= fun _ ->
    from_lwt(Lwt_unix.read fd buf 0 blk_sz) >>= fun n ->
    assert(n=0 || n=blk_sz);
    return buf

  let write_blk fd blk_index buf = 
    assert(Bytes.length buf = blk_sz);
    from_lwt (Lwt_unix.lseek fd (blk_sz*blk_index) SEEK_SET) >>= fun _ ->
    from_lwt (Lwt_unix.write fd buf 0 blk_sz) >>= fun n ->
    assert(n=blk_sz);
    return ()    

  let lwt_file_ops = { read_file; write_string_to_file; stat; open_; close; blk_sz; read_blk; write_blk }
end

open Buf_factory.Buf_as_bigarray

(** This version standardizes on ba_buf, but Lwt prefers bytes... so this is slightly inefficient *)
module Lwt_file_ops__ba_buf : sig
  type fd = Lwt_unix.file_descr
  type blk = ba_buf
  val lwt_file_ops : (fd,blk,lwt) file_ops
end = struct 
  open Tjr_monad.With_lwt

  include Lwt_file_ops__bytes

  let { read_file; write_string_to_file; stat; open_; close; blk_sz; read_blk; write_blk }  = lwt_file_ops

  type blk = ba_buf

  (* let buf_ops = Buf_factory.Buf_as_bigarray.ba_buf_ops *)
      
  let read_blk fd blk_index = 
    read_blk fd blk_index >>= fun blk ->
    return (Bigstring.of_bytes blk)

  let write_blk fd blk_index buf = 
    write_blk fd blk_index (Bigstring.to_bytes buf)

  let lwt_file_ops = { read_file; write_string_to_file; stat; open_; close; blk_sz; read_blk; write_blk }
end

(** default to using bigarray *)
let lwt_file_ops = Lwt_file_ops__ba_buf.lwt_file_ops
