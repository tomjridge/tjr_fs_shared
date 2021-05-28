(** A basic implementation of a block device (lwt). 

NOTE don't access this directly; used blk_devs#lwt instead.
*)

(** NOTE there are various options:
- monad (store passing, imperative, lwt etc)
- block implementation (string, buf etc)
- read/write operations (interacts with the monad; lwt_unix or unix)

*)

open Blk_intf

(** NOTE lwt uses bytes, but we tend to prefer bigstring for
   integration with FUSE *)
module With_(S:sig 
    type blk = Shared_ctxt.blk
    val blk_sz       : blk_sz 
    val bytes_to_blk : bytes -> blk
    val blk_to_bytes : blk -> bytes
  end) = struct

  open S
  open Lwt
      
  let from_lwt = Tjr_monad.With_lwt.from_lwt

  module LU = Lwt_unix 
    

  module B = Blk_id_as_int
  type blk_id = B.blk_id

  let blk_sz_i = Blk_sz.to_int blk_sz

  let _ : unit = assert(blk_sz_i > 0)

  let from_fd fd = 
    let open (struct
      let read ~(blk_id:blk_id) = 
        let x = 
          let blk_id = B.to_int blk_id in
          assert(blk_id>=0);
          let buf = Bytes.make blk_sz_i (Char.chr 0) in  
          (* NOTE pread and pwrite are used to avoid locking at the lwt layer *)
          LU.pread fd buf ~file_offset:(blk_id * blk_sz_i) 0 blk_sz_i >>= fun n -> 
          (* assert (n=blk_sz); we allow the file to expand automatically, so
             no reason to read any bytes since file could be empty *)
          assert(n=0 || n=blk_sz_i);
          bytes_to_blk buf |> return
        in
        x |> from_lwt

      let write ~(blk_id:blk_id) ~(blk:blk) =
        let x = 
          let blk_id = B.to_int blk_id in
          let buf = blk |> blk_to_bytes in
          assert(Bytes.length buf > 0);
          assert(blk_id>=0);           
          (* assert(not !in_use); *)
          (* in_use:=true; *)
          LU.pwrite fd buf ~file_offset:(blk_id * blk_sz_i) 0 blk_sz_i >>= fun n -> 
          (* Printf.printf "%d %d\n%!" (String.length blk) blk_sz; *)
          (* test(fun _ -> assert (n=blk_sz)); *)
          assert (n=blk_sz_i);
          (* in_use:=false; *)
          return ()
        in
        x |> from_lwt

      (* FIXME this can probably be made more efficient *)
      let write_many ws = 
        let open Tjr_monad.With_lwt in
        ws |> iter_k (fun ~k ws -> match ws with
              | [] -> return ()
              | (blk_id,blk)::ws -> 
                write ~blk_id ~blk >>= fun () ->
                k ws)

      let blk_dev_ops : (_,Shared_ctxt.blk,_)blk_dev_ops = {
        blk_sz;
        read;
        write;
        write_many
      }

      let sync () = 
        let x = 
          LU.fsync fd
        in
        x |> from_lwt

      let close () = 
        let x = LU.close fd in
        x |> from_lwt

    end)
    in
    object
      method blk_dev_ops=blk_dev_ops
      method fd=fd
      method sync=sync
      method close=close
    end

  let _ = from_fd

end

let lwt_impl : (_,Shared_ctxt.blk,_,_) blk_dev_impl = object
  method add_debug=add_debug
  method add_profiling=fun blk_dev_ops -> 
    add_profiling ~monad_ops:With_lwt.monad_ops ~blk_dev_ops
  method with_=fun ~blk_sz -> 
    let module X = With_(struct
        type blk=Shared_ctxt.blk
        let blk_sz=blk_sz
        let bytes_to_blk=(fun x -> Shared_ctxt.{ba_buf=Bigstring.of_bytes x})
        let blk_to_bytes=(fun (x:Shared_ctxt.blk) -> Bigstring.to_bytes x.Shared_ctxt.ba_buf)
      end)
    in
    object 
     method from_fd=X.from_fd
   end
end
                           
