(** Represent common context using classes; don't open (use [Tjr_fs_shared.sh_std_ctxt]) *)


class virtual ['r,'blk,'buf,'t] sh_ctxt = object
  method virtual r_cmp : 'r -> 'r -> int
  method virtual r_size :int
  method virtual buf_ops:'buf Buf_ops.buf_ops
  method virtual monad_ops: 't monad_ops
  method virtual async: 't async
  method virtual event_ops: 't event_ops
  method virtual blk_ops: 'blk Blk_intf.blk_ops
  method virtual blk_sz : Blk_intf.blk_sz
  method virtual buf_to_blk: 'buf -> 'blk
  method virtual blk_to_buf: 'blk -> 'buf
  method virtual buf_create: unit -> 'buf
end

module Std = struct
  (** Common types we use in examples 

      Standard types: t = lwt; blk=ba_buf; r=blk_id 

  *)

  [@@@warning "-32"]

  open Buf_ops
  open Blk_intf

  type t = lwt
  type blk = ba_buf
  type blk_id = Blk_intf.Blk_id_as_int.blk_id[@@deriving bin_io, yojson]
  type r = blk_id[@@deriving bin_io, yojson]
  type buf = ba_buf

  module B = Blk_intf.Blk_id_as_int

  (** NOTE we typically abbreviatte when there are 3 or more params *)

  (* $(CONVENTION("If we define a type abbrev for a parameterized
     type, use a prime as suffix, so we don't get confused with the
     parameterized type")) *)

  type blk_dev_ops' = (blk_id,blk,t)blk_dev_ops
  (* type nonrec blk_allocator_ops = (r,t)blk_allocator_ops *)


  let monad_ops = lwt_monad_ops
  let async = With_lwt.async
  let event_ops = With_lwt.event_ops

  let r_cmp : r -> r -> int = Stdlib.compare
  let r_size = 9
  let r_cmp : r -> r -> int = Stdlib.compare
  let r_size = 9 (* max size of r=blk_id when marshalled *)

  let buf_ops = Buf_factory.Buf_as_bigarray.ba_buf_ops
  let blk_ops = Blk_factory.make_3()
  let blk_sz = Blk_intf.blk_sz_4096
  let buf_to_blk : buf->blk = fun x -> x
  let blk_to_buf : blk->buf = fun x -> x
  let buf_create = fun () -> buf_ops.create (Blk_sz.to_int blk_sz)

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

  class sh_std_ctxt = object
    inherit [r, blk, buf, t] sh_ctxt
    method r_cmp = r_cmp
    method r_size = r_size
    method buf_ops = buf_ops
    method monad_ops = monad_ops
    method async = async
    method event_ops = event_ops
    method blk_ops = blk_ops
    method blk_sz = blk_sz
    method buf_to_blk = buf_to_blk
    method blk_to_buf = blk_to_buf
    method buf_create = buf_create
  end

end

class sh_std_ctxt = Std.sh_std_ctxt
