(** Common blk devs; don't open *)


let blk_devs = 
  let lwt_impl = Blk_dev_impl_lwt.lwt_impl in
  (* NOTE this is convenience *)
  let lwt_open_file ~fn ~create ~trunc = 
    let open With_lwt in
    let open Shared_ctxt in
    (* The following combines the fd interface with the blkdev intf *)
    lwt_file_ops#open_file ~fn ~create ~trunc >>= fun fd ->
    lwt_impl#with_ ~blk_sz |> fun o -> 
    o#from_fd fd |> fun bd ->
    return bd
  in
  object
    method lwt=lwt_impl
    method lwt_open_file=lwt_open_file
    method in_mem=Blk_dev_impl_in_mem.make_blk_dev_in_mem
  end


(* let default_create_perm = Tjr_file.default_create_perm *)


(*
class type open_fd = 
  object
    method fd          : Lwt_unix.file_descr
    method blk_dev_ops : (Shared_ctxt.blk_id,Shared_ctxt.blk,Shared_ctxt.t) blk_dev_ops
    method sync        : unit -> (unit,Shared_ctxt.t)m
    method close       : unit -> (unit,Shared_ctxt.t)m
  end
*)

(*
  let open (struct
    open Shared_ctxt
    open With_lwt

    (* type ('k,'v)stdmap = ('k,'v) Tjr_map.With_stdcmp.stdmap *)
        
    (* $(TODO("""add a profiler print header which takes a string,
       short suffix eg bt-ex and file""")) *)


    let blk_devs =   
      object
        method add_profiling = add_profiling
        method add_debug = add_debug
        method with_ba_buf = object (s)
          method from_filename = fun ~fn ~create ~init -> 
            let open Lwt_unix in       
            let flgs = 
              (if create then [O_CREAT] else [])@
              (if init then [O_TRUNC] else [])@
              [O_RDWR] 
            in
            from_lwt (openfile fn flgs default_create_perm) >>= fun fd ->
            return (s#from_fd fd)
          method from_fd = fun fd -> 
            Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd |> fun blk_dev_ops ->            
            let o : open_fd = object
              method fd = fd
              method blk_dev_ops = blk_dev_ops
              method sync () = from_lwt (Lwt_unix.fsync fd)
              method close () = from_lwt(Lwt_unix.close fd)
            end
            in
            o
        end
      end
      
  end)
  in
  blk_devs
*)


(*      
    method with_string = object
      method from_fd = make_1
    end
    method with_bytes = object
      method from_fd = make_2
    end
    method of_stdmap () = make_3 ()
    method with_bigstring = object
      method from_fd = make_5
    end
    method as_fc_mod = object
      method from_fd = make_7
      method from_fn = make_8
    end
    method from_fn = make_9
*)
      
(*
(* We get back a blk_dev and a function for closing the blk dev, by
   closing the underlying fd *)
module type R6 = sig
  open Shared_ctxt
  val fd : Lwt_unix.file_descr
  val close_blk_dev : unit -> (unit, lwt) m
  val blk_dev_ops : (blk_id, ba_buf, lwt) blk_dev_ops
end
(* FIXME make the other args return a similar result *)
*)


(*

    

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
        
    (* FIXME same as make_3? *)
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
  end)
*)
