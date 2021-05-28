(** Represent common context using classes; don't open (use [Tjr_fs_shared.ctxt#shared]) *)

include Shared_ctxt_summary

open Buf_ops
open Blk_intf

(* $(PIPE2SH("""sed -n '/type[ ].*shared_ctxt = /,/^}/p' >GEN.shared_ctxt.ml_ """)) *)
type ('r,'blk,'buf,'t) shared_ctxt = {
  r_cmp      : 'r -> 'r -> int;
  r_size     :int;
  buf_ops    :'buf buf_ops;
  monad_ops  : 't monad_ops;
  async      : 't async;
  event_ops  : 't event_ops;
  blk_ops    : ('blk,'buf) blk_ops;
  blk_sz     : blk_sz;
  buf_to_blk : 'buf -> 'blk;
  blk_to_buf : 'blk -> 'buf;
  buf_create : unit -> 'buf;
}

open Buf_ops
open Blk_intf

(** Common types we use in examples 

    Standard types: t = lwt; blk=buf=ba_buf; r=blk_id=blk_id_as_int

*)

[@@@warning "-32"]

type blk = { ba_buf: ba_buf }
type buf = { ba_buf: ba_buf; mutable is_valid: bool }
let ba_buf_to_buf ba_buf = {ba_buf;is_valid=true}
let ba_buf_to_blk ba_buf = {ba_buf}


type t      = lwt
(* type blk    = ba_buf *)
type blk_id = Blk_intf.Blk_id_as_int.blk_id[@@deriving bin_io, yojson, sexp]
type r      = blk_id[@@deriving bin_io, yojson, sexp]
(* type buf    = ba_buf *)

(** Abbreviation *)
module B = Blk_intf.Blk_id_as_int

(** NOTE we typically abbreviate types when there are 3 or more params *)

(* $(CONVENTION("""If we define a type abbrev for a parameterized
   type, use a prime as suffix, so we don't get confused with the
   parameterized type; don't define an abbrev if only 2 params or
   less""")) *)

let monad_ops = lwt_monad_ops

let ( >>= ) = monad_ops.bind

let return = monad_ops.return

(* $(FIXME(""" add mutexops and yield to the shared_ctxt; perhaps add a monadic ctxt which includes all the values related to the monad""")) *)
type mutex = With_lwt.mutex
type cvar = With_lwt.cvar
let mutex_ops = With_lwt.mutex_ops

let async = With_lwt.async

let yield () = With_lwt.(yield () |> from_lwt)

let event_ops = With_lwt.event_ops

let r_cmp : r -> r -> int = Stdlib.compare

let r_size = 9 (* max size of r=blk_id when marshalled *)

(* FIXME really we should treat buffers as properly mutable, rather
   than via value passing and linearity *)
let buf_ops : buf buf_ops = 
  let x = Buf_ops.buf_ops#ba in
  {
    buf_create=(fun n -> x.buf_create n |> ba_buf_to_buf);
    buf_length=(fun {ba_buf;is_valid} -> 
        assert(is_valid);
        x.buf_length ba_buf);
    buf_get=(fun i {ba_buf;is_valid} -> 
        assert(is_valid);
        x.buf_get i ba_buf);
    buf_to_string=(fun ~src ~off ~len ->
        assert(src.is_valid);
        x.buf_to_string ~src:src.ba_buf ~off ~len);
    to_string=(fun {ba_buf;is_valid} -> 
        assert(is_valid);
        x.to_string ba_buf);
    of_string=(fun s -> s |> x.of_string |> ba_buf_to_buf);
    to_bytes=(fun {ba_buf;is_valid} -> 
        assert(is_valid);
        x.to_bytes ba_buf);
    of_bytes=(fun bs -> bs |> x.of_bytes |> ba_buf_to_buf);
    of_ba=(fun ba -> ba_buf_to_buf ba);
    buf_sub=(fun ~buf ~off ~len -> 
        (* FIXME should this share? *)
        assert(buf.is_valid);
        buf.is_valid <- false;
        x.buf_sub ~buf:buf.ba_buf ~off ~len |> ba_buf_to_buf);
    blit=(fun ~src ~src_off ~src_len ~dst ~dst_off -> 
        assert(src.is_valid);
        assert(dst.is_valid);
        dst.is_valid <- false;
        x.blit ~src:src.ba_buf ~src_off ~src_len ~dst:dst.ba_buf ~dst_off |> ba_buf_to_buf);
    blit_bytes_to_buf=(fun ~src ~src_off ~src_len ~dst ~dst_off ->
        assert(dst.is_valid);
        dst.is_valid <- false;
        x.blit_bytes_to_buf ~src ~src_off ~src_len ~dst:dst.ba_buf ~dst_off |> ba_buf_to_buf);
    blit_string_to_buf=(fun ~src ~src_off ~src_len ~dst ~dst_off -> 
        assert(dst.is_valid);
        dst.is_valid <- false;
        x.blit_string_to_buf ~src ~src_off ~src_len ~dst:dst.ba_buf ~dst_off |> ba_buf_to_buf);
  }
        
                                                        

let blk_sz = Blk_intf.blk_sz_4096

let blk_ops : (blk,buf)blk_ops = 
  { blk_sz;
    blk_to_buf=(fun {ba_buf} -> { ba_buf;is_valid=true});
    buf_to_blk=(fun buf -> 
        assert(buf.is_valid);
        buf.is_valid <- false;
        {ba_buf=buf.ba_buf})
  }
(* was Blk_impls.blk_ops#ba_ba_4096 *)
        

let blk_sz_i = Blk_sz.to_int blk_sz

let {buf_to_blk;blk_to_buf;_} = blk_ops

(* FIXME remove buf_ops in favour of buf_create and buf_length? *)
(* NOTE this specializes buf_ops.buf_create to blk_sz *)
let buf_create = fun () -> buf_ops.buf_create (Blk_sz.to_int blk_sz)

(* FIXME this is for testing only; rename? move? *)
let make_blk_allocator: blk_id ref -> (r,t)blk_allocator_ops = fun b_ref ->    
  let open With_lwt in
  let blk_alloc () = 
    let r = !b_ref in
    B.incr b_ref;
    return r
  in
  let blk_free blk_id = 
    (* FIXME do nothing for this simple allocator *)
    return ()
  in
  { blk_alloc; blk_free }

(* $(CONVENTION("""Build up ctxts using a prefix + "_ctxt". Then add
   as methods on a global ctxt object (which gets augmented in future
   libraries with further methods). """)) *)

let shared_ctxt =
  {
    r_cmp;
    r_size;
    buf_ops;
    monad_ops;
    async;
    event_ops;
    blk_ops;
    blk_sz;
    buf_to_blk;
    blk_to_buf;
    buf_create;
  }

let ctxt = object
  method shared_ctxt=shared_ctxt
end
