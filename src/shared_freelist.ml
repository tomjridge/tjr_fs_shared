(** This is a shared type for blk freelists, and a dummy
   implementation; other libraries may have their own; don't open *)

(* $(PIPE2SH("""sed -n '/type[ ].*freelist_ops = /,/}/p' >GEN.freelist_ops.ml_ """)) *)
type ('blk_id,'t) freelist_ops = {
  alloc      : unit -> ('blk_id,'t)m;
  alloc_many : int -> ('blk_id list,'t)m;
  free       : 'blk_id -> (unit,'t)m;
  free_many  : 'blk_id list -> (unit,'t)m;
  sync       : unit -> (unit,'t)m;
  (** NOTE the freelist already ensures it is crash safe; this sync is
      really for tidy shutdown *)
}

(** NOTE these are just mocks, for testing *)
(* $(PIPE2SH("""sed -n '/type[ ].*freelist_factory = /,/^>/p' >GEN.freelist_factory.ml_ """)) *)
type ('blk_id,'t,'internal_state) freelist_factory = <
  with_:
    fn:string -> 
    <
      create: min_free:'blk_id -> ( <
          close : unit -> (unit,'t)m;
          freelist_ops: ('blk_id,'t)freelist_ops;
          get_state : unit -> 'internal_state; (* debug only *)
        >,'t)m;
      (** Create a new freelist *)

      restore: unit -> ( <
          close : unit -> (unit,'t)m;
          freelist_ops: ('blk_id,'t)freelist_ops;
          get_state : unit -> 'internal_state; (* debug only *)
        >,'t)m;
      (** Restore from file *)

    >      
>

(** Simple example, with just a "min free" int, backed by a file; good
   intro to how the freelist actually works *)
module Example = struct

  open Shared_ctxt
  open With_lwt

  type t = {
    min_free : int;
    last     : int; (* last synced min free, >= min_free *)
  }

  module On_disk = struct
    type t = { min_free: int } 
  end

  let delta = 1000 (* amount we increase last_synced_min_free by *)

  module With_filename(S:sig val fn: string end) = struct
    open S

    let write_state fd t = 
      Printf.printf "%s: writing state\n" __FILE__;
      let { last; _ } = t in
      let o = On_disk.{min_free=last} in
      Marshal.to_bytes o [] |> fun buf -> 
      assert(Bytes.length buf > 0);
      lwt_file_ops#pwrite ~fd ~off:0 ~buf

    let ops fd with_state = 
      let alloc () = 
        with_state.with_state (fun ~state ~set_state -> 
          let x,y = state.min_free, state.last in
          match x >= y with
          | true -> 
            assert (x=y);
            (* increment last, write then alloc *)
            let y' = y + delta in
            let state = {state with last=y'} in
            let to_return = B.of_int state.min_free in
            let state = {state with min_free=state.min_free+1} in
            write_state fd state >>= fun () -> 
            set_state state >>= fun () -> 
            return to_return
          | false -> 
            let to_return = B.of_int state.min_free in
            let state = {state with min_free=state.min_free+1} in
            (* NOTE in the common case, we don't have to go to disk 
               write_state fd state >>= fun () ->  *)
            set_state state >>= fun () -> 
            return to_return)
      in
      let alloc_many n = 
        (0,[]) |> iter_k (fun ~k (i,xs) -> 
            match i>=n with
            | true -> return xs
            | false -> 
              alloc () >>= fun r -> 
              k (i+1,r::xs))
      in
      let free r = return () in
      let free_many rs = return () in
      let sync () = 
        with_state.with_state (fun ~state ~set_state -> 
            (* NOTE may want to sync the file here as well... but this
               is only a demo *)
            write_state fd state)
      in
      { alloc; alloc_many; free; free_many; sync }
          
    let create ~min_free = 
      let open (struct
        let min_free = B.to_int min_free

        let fd = lwt_file_ops#open_file ~fn ~create:true ~trunc:true
            
        let t = { min_free; last=min_free }

        (* sync the state *)
        let res = 
          fd >>= fun fd -> 
          write_state fd t

        (* and return the freelist *)

        let t = ref t

        let with_state = with_imperative_ref ~monad_ops t

        let close () = 
          fd >>= fun fd -> lwt_file_ops#close fd

        let res = 
          fd >>= fun fd -> 
          res >>= fun () -> 
          let freelist_ops = ops fd with_state in
          let get_state () = !t in
          return (object
            method close=close
            method freelist_ops=freelist_ops
            method get_state=get_state
          end)
      end)
      in
      res

    let restore () = 
      let open (struct
        let fd = lwt_file_ops#open_file ~fn ~create:false ~trunc:false
                    
        (* assume we can marshal in 64 bytes *)
        let buf = 
          fd >>= fun fd -> lwt_file_ops#pread ~fd ~off:0 ~len:64

        let ops = 
          buf >>= fun buf -> 
          fd >>= fun fd -> 
          let open (struct
            let o = Marshal.from_bytes buf 0 
            let min_free = o.min_free 
            let t = {min_free; last=min_free} 
            let t = ref t
            let with_state = with_imperative_ref ~monad_ops t
            let freelist_ops = ops fd with_state
            let get_state () = !t
            let close () = lwt_file_ops#close fd
            let to_return = object
              method close=close
              method freelist_ops=freelist_ops
              method get_state=get_state
            end
          end)
          in
          return to_return
      end)
      in
      ops

    let obj = object
      method create=create
      method restore=restore
    end

  end (* With_filename *)

  let example : (Shared_ctxt.r, _,_) freelist_factory = object
    method with_ ~fn = 
      let open With_filename (struct let fn=fn end) in
      obj
  end

end
  
let example = Example.example


module Test() = struct
  open Shared_ctxt
  open With_lwt
  open Example

  let () = Printf.printf "shared_freelist: starting tests\n%!"

  let fn = "shared_freelist_test.marshalled"

  let x = example#with_ ~fn

  let delta = Example.delta

  (* create a freelist *)
  let () = 
    begin
      x#create ~min_free:(B.of_int 4) >>= fun y -> 
      assert(y#get_state () = { min_free=4;last=4 });
      y#close ()
    end
    |> to_lwt |> Lwt_main.run 

  (* restore a freelist *)
  let () =
    begin
      x#restore () >>= fun y -> 
      assert(y#get_state () = { min_free=4;last=4 });
      y#close ()
    end
    |> to_lwt |> Lwt_main.run 

  (* restore, allocate *)
  let () =
    begin
      x#restore () >>= fun y -> 
      assert(y#get_state () = { min_free=4;last=4 });
      y#freelist_ops.alloc_many 1000 >>= fun xs -> 
      assert(List.rev xs=(List_.from_upto 4 (4+1000) |> List.map B.of_int));
      y#get_state () |> fun s -> 
      (* NOTE the following depend on delta=1000 *)
      assert(s.min_free=4+1000); 
      assert(s.last=4+1000);
      y#close ()
    end
    |> to_lwt |> Lwt_main.run 
    
  let () = Printf.printf "shared_freelist: tests pass\n%!"

end
