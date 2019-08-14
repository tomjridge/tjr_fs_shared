(** A basic implementation of a block device. *)

(** NOTE there are various options:
- monad (store passing, imperative, lwt etc)
- block implementation (string, buf etc)
- read/write operations (interacts with the monad; lwt_unix or unix)

*)

open Blk_intf

(* NOTE the blk_id is a plain int *)
module Internal_unix 
: sig
  val read :
    blk_ops:'blk blk_ops -> fd:Unix.file_descr -> unit -> blk_id:int -> 'blk
  val write :
    blk_ops:'blk blk_ops ->
    fd:Unix.file_descr -> unit -> blk_id:int -> blk:'blk -> unit
end
= struct
  let read ~(blk_ops:'blk blk_ops) ~fd () = 
    let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
    fun ~blk_id -> 
      assert(blk_id>=0);
      ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
      let buf = Bytes.make blk_sz (Char.chr 0) in 
      let n = Unix.read fd buf 0 blk_sz in
      (* assert (n=blk_sz); we allow the file to expand automatically, so
         no reason to read any bytes since file could be empty *)
      (* test(fun _ -> assert(n=0 || n=blk_sz)); *)
      assert(n=0 || n=blk_sz);
      Bytes.to_string buf |> blk_ops.of_string 

  let write ~(blk_ops:'blk blk_ops) ~fd () = 
    let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
    assert(blk_sz > 0);
    fun ~blk_id ~blk -> 
      let blk = blk_ops.to_string blk in
      assert(String.length blk > 0);
      assert(blk_id>=0);
      ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
      let buf = blk |> Bytes.of_string in
      (* Printf.printf "%d %d\n%!" (String.length blk) blk_sz; *)
      let n = Unix.single_write fd buf 0 blk_sz in
      (* test(fun _ -> assert (n=blk_sz)); *)
      assert(n=blk_sz);
      ()
end

module Make(Blk_id_:sig type blk_id val to_int: blk_id -> int (* val of_int: int -> blk_id *) end)
: sig
  val make_with_lwt :
    blk_ops:'a blk_ops ->
    fd:Lwt_unix.file_descr ->
    (Blk_id_.blk_id, 'a, Tjr_monad.With_lwt.lwt) blk_dev_ops
  val make_with_unix :
    monad_ops:'a monad_ops ->
    blk_ops:'b blk_ops ->
    fd:Unix.file_descr -> (Blk_id_.blk_id, 'b, 'a) blk_dev_ops
end
 = struct
  module Internal = struct
    module Unix_ = struct
      let read ~monad_ops ~(blk_ops:'blk blk_ops) ~fd () = 
        let read = Internal_unix.read ~blk_ops ~fd () in
        let return = monad_ops.return in
        let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
        fun ~blk_id -> 
          read ~blk_id |> return

      let write ~monad_ops ~(blk_ops:'blk blk_ops) ~fd () = 
        let write = Internal_unix.write ~blk_ops ~fd () in
        let return = monad_ops.return in
        let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
        assert(blk_sz > 0);
        fun ~blk_id ~blk -> 
          write ~blk_id ~blk |> return
    end

    module Lwt_ = struct
      module UU = Lwt_unix 

      open Lwt

      let read ~(blk_ops:'blk blk_ops) =
        let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
        fun ~fd ~blk_id -> 
          begin
            assert(blk_id>=0);
            UU.lseek fd (blk_id * blk_sz) SEEK_SET >>= fun _ -> 
            let buf = Bytes.make blk_sz (Char.chr 0) in 
            UU.read fd buf 0 blk_sz >>= fun n -> 
            (* assert (n=blk_sz); we allow the file to expand automatically, so
               no reason to read any bytes since file could be empty *)
            (* test(fun _ -> assert(n=0 || n=blk_sz)); *)
            assert(n=0 || n=blk_sz);
            Bytes.to_string buf |> blk_ops.of_string |> return
          end
          |> Tjr_monad.With_lwt.from_lwt

      let _ = read

      let write ~(blk_ops:'blk blk_ops) = 
        let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
        fun ~fd ~blk_id ~blk -> 
          begin
            let blk = blk_ops.to_string blk in
            assert(String.length blk > 0);
            assert(blk_sz > 0);
            assert(blk_id>=0);           
            UU.lseek fd (blk_id * blk_sz) SEEK_SET >>= fun _ -> 
            let buf = blk |> Bytes.of_string in
            (* Printf.printf "%d %d\n%!" (String.length blk) blk_sz; *)
            UU.write fd buf 0 blk_sz >>= fun n -> 
            (* test(fun _ -> assert (n=blk_sz)); *)
            assert (n=blk_sz);
            return ()
          end
          |> Tjr_monad.With_lwt.from_lwt
    end

  end


  module With_lwt = struct
    module L = Internal.Lwt_
    (** Construct a naive [blk_dev_ops] backed by a file. For testing. *)
    let make_blk_dev_on_fd ~(blk_ops:'blk blk_ops) ~fd = 
      let blk_sz = blk_ops.blk_sz in
      let read ~blk_id = 
        L.read ~blk_ops ~fd ~blk_id:(Blk_id_.to_int blk_id) 
      in
      let write ~blk_id ~blk = 
        L.write ~blk_ops ~fd ~blk_id:(Blk_id_.to_int blk_id) ~blk 
      in
      { blk_sz; read; write }
  end

  module With_unix = struct
    module U = Internal.Unix_

    (** Construct a naive [blk_dev_ops] backed by a file. For testing. *)
    let make_blk_dev_on_fd ~monad_ops ~(blk_ops:'blk blk_ops) ~fd = 
      let blk_sz = blk_ops.blk_sz in
      let read = U.read ~monad_ops ~blk_ops ~fd () in
      let read ~blk_id = read ~blk_id:(Blk_id_.to_int blk_id) in
      let write = U.write ~monad_ops ~blk_ops ~fd () in
      let write ~blk_id ~blk = write ~blk_id:(Blk_id_.to_int blk_id) ~blk in
      { blk_sz; read; write }
  end

  module Export = struct
    let make_with_lwt = With_lwt.make_blk_dev_on_fd
    let make_with_unix = With_unix.make_blk_dev_on_fd
  end
  include Export
end

module With_blk_id_as_int = Make(Blk_id_as_int)

include With_blk_id_as_int
