(** Common blk devs; don't open *)

open Blk_intf
(* open Buf_ops *)

let default_create_perm = Tjr_file.default_create_perm

let blk_devs = 
  let open (struct
    open Shared_ctxt
    open With_lwt

    (* type ('k,'v)stdmap = ('k,'v) Tjr_map.With_stdcmp.stdmap *)
        
    (* $(TODO("""add a profiler print header which takes a string,
       short suffix eg bt-ex and file""")) *)

    let add_profiling blk_dev_ops = 
      let 
        [r1; w1; w2 ] =
        ["r1" ; "w1"; "w2" ]
        |> List.map Tjr_profile.intern[@@warning "-8"]
      in
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


    let add_debug (blk_dev_ops:(blk_id,_,_)blk_dev_ops) = 
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

    let _ = add_debug

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
            s#from_fd fd
          method from_fd = fun fd -> 
            Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd |> fun blk_dev_ops ->            
            return (object
              method fd = fd
              method blk_dev_ops = blk_dev_ops
              method sync () = from_lwt (Lwt_unix.fsync fd)
              method close () = from_lwt(Lwt_unix.close fd)
            end)
        end
      end
      
  end)
  in
  blk_devs

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
