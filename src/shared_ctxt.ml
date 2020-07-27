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

type t      = lwt
type blk    = ba_buf
type blk_id = Blk_intf.Blk_id_as_int.blk_id[@@deriving bin_io, yojson, sexp]
type r      = blk_id[@@deriving bin_io, yojson, sexp]
type buf    = ba_buf

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

let async = With_lwt.async

let event_ops = With_lwt.event_ops

let r_cmp : r -> r -> int = Stdlib.compare

let r_size = 9 (* max size of r=blk_id when marshalled *)

let buf_ops = Buf_ops.buf_ops#ba

let blk_ops = Blk_impls.blk_ops#ba_ba_4096

let blk_sz = Blk_intf.blk_sz_4096

let blk_sz_i = Blk_sz.to_int blk_sz

(* FIXME part of blk_ops?  *)
let buf_to_blk : buf->blk = fun x -> x

let blk_to_buf : blk->buf = fun x -> x

(* FIXME remove buf_ops in favour of buf_create and buf_length? *)
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

let shared_ctxt = {
  r_cmp = r_cmp;
  r_size = r_size;
  buf_ops = buf_ops;
  monad_ops = monad_ops;
  async = async;
  event_ops = event_ops;
  blk_ops = blk_ops;
  blk_sz = blk_sz;
  buf_to_blk = buf_to_blk;
  blk_to_buf = blk_to_buf;
  buf_create = buf_create;
}

let ctxt = object
  method shared_ctxt=shared_ctxt
end
