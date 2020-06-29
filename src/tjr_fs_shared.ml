(** Common types and defns for file system work.

A collection of the main types provided by this library. *)


(** {2 A record for the pair of an initial state and related operations} *)

type ('a,'b) initial_state_and_ops = ('a,'b) Internal_intf.initial_state_and_ops = {initial_state:'a; ops:'b}


(** {2 Int-like types} *)

module Int_like = Int_like
(* don't include, since we are in the process of moving to these types *)


(** {2 Buffers} *)

include Buf_ops

module Buf_factory = Buf_factory
open Buf_factory
module Buf_as_bigarray = Buf_as_bigarray
module Buf_as_bytes = Buf_as_bytes

let ba_buf_ops = Buf_as_bigarray.ba_buf_ops

let by_buf_ops = Buf_as_bytes.by_buf_ops

module Buffers_from_btree = Buffers_from_btree (* FIXME combine with previous *)


(** {2 Block-related types } *)

include Blk_intf


(** {2 Block-related implementations} *)

(* FIXME remove; use blk_factory *)
(* module Common_blk_ops = Common_blk_ops *)

module Blk_factory = Blk_factory 

(*
(* FIXME remove, use blk_dev_factory *)
module Blk_dev_in_mem = Blk_dev_in_mem

module Blk_dev_on_fd = Blk_dev_on_fd
*)

(* FIXME remove *)
(* module Common_blk_layers = Common_blk_layers *)

(* module Common_blk_stores = Common_blk_stores *)

module Blk_dev_factory = Blk_dev_factory
type open_fd = Blk_dev_factory.open_fd
let blk_devs = Blk_dev_factory.blk_devs


module Root_block = Root_block



(** {2 Operations extended with a "sync" operation} *)

(** This is a common requirement on operations from the "lower" layer:
   there is a need to push changes that have been submitted to the
   lower layer, so that they are actually persistent. Maybe "flush"
   would be a better name. *)
type ('a,'t) with_sync = <
  get: 'a;
  sync: unit -> (unit,'t)m
>


(** {2 Note on sync and the blk device layer} *)

(**

A traditional block device accepts (read and) write requests. Write
   requests can be reordered, although writes for a particular blk id
   will commit in order.

A barrier operation enforces that it is not the case that a request
   before the barrier is reordered with a request after the barrier.

A sync operation requests that all writes are actually made
   persistent.

Of course, many devices do not implement these things properly.

Newer interfaces, such as (variants of) TCQ and NVMe, are more
   complicated. Typically there are queues of operations. Barrier and
   flush commands can be per-queue. It may be possible to request
   notification for when some operation actually completes (say, when
   a write is made persistent). There is often a "Forced Unit Access
   (FUA)" mode, which attempts to avoid caching etc and just persist
   the write. It may also be possible to provide additional
   information about the "priority" of a write, or even a "time out"
   associated with a write (if it hasn't happened by this time, then
   don't perform the write at all).

Effectively, a queue operates like a traditional block device.

Traditional block devices are likely to be very slow; queue-based
   interfaces such as those used in NVMe, are likely to be a much
   better fit for ImpFS. Suppose we have a persistent object on which
   we call the "sync" method. Then a traditional blk layer may already
   have some cached writes related to that object. If the blk layer
   does not know the connection between these writes and the object,
   then a sync on the object must force a complete sync of the blk
   layer (including all writes to all objects) which is potentially
   very costly. If we assume a queue-based interface, then things are
   much better, because the sync can target a particular queue (and
   all writes for the object will be placed in that queue).

In ImpFS, each object is provided with an interface that appears like
   a traditional block device (with sync and possibly barrier
   operations). However, the real block layer is assumed to be queue
   based. Our current implementations of this on top of traditional
   block devices are very inefficient because we effectively have only
   one queue. However, we expect that porting the code to use a more
   advanced queue-based interface would give a much greater
   performance.

It is perhaps worth putting some effort into emulating the queue-based
   interface efficiently on top of a traditional block device. I'm not
   sure how this should best be achieved, or whether it would actually
   improve performance.

*)


(** {2 Kv ops} *)

module Kvop = Kvop
type ('k,'v)kvop = ('k,'v)Kvop.kvop
module Kvop_map = Kvop.Kvop_map

(* include Kvop.Kv_op_type *)

(*
(* NOTE we don't want to pollute the namespace with all the @@deriving
   functions so we dont include Kv_op_type directly *)
type ('k,'v) kvop = ('k,'v) Kv_op_type.kvop = 
  | Insert of 'k * 'v
  | Delete of 'k
    [@@deriving bin_io, yojson]

type ('k,'v) kvop_map = ('k,'v) Kv_op_type.kvop_map
module Kv_op = Kv_op
*)

(** {2 (Shared) Map ops } *)

(** FIXME do we want to include this type at top level? *)
(* include Map_ops_type *)

(** Map operations find,ins,del, in; insert_all; make_insert_many FIXME? note may clash with other "map_ops" so we don't include at the top-level *)
(* module Map_ops = Map_ops *)
module Shared_map_ops = Shared_map_ops


(** {2 Small strings, leq 256 bytes} *)

(* FIXME remove this *)
(* type ss = Small_string.ss *)
(* module Small_string = Small_string *)

module Str_256 = Str_256
type str_256 = Str_256.str_256


(** {2 Maps with key traversal: get_next_binding, get_prev_binding} *)

module Map_with_key_traversal = Map_with_key_traversal


(** {2 Write back cache} *)

module Write_back_cache_v1_DONT_USE = Write_back_cache

module Write_back_cache_v2_DONT_USE = Write_back_cache_v2

module Write_back_cache_v3 = Write_back_cache_v3

module Write_back_cache = Write_back_cache_v3

type wbc_params = Write_back_cache.wbc_params
type ('k,'v,'t) wbc_ops = ('k,'v,'t) Write_back_cache.wbc_ops
type ('k,'v,'t) wbc_ops_plus = ('k,'v,'t) Write_back_cache.wbc_ops_plus
type ('k,'v,'t) wbc_factory = ('k,'v,'t) Write_back_cache.wbc_factory

(** {2 File operations} *)

module File_ops = File_ops

let lwt_file_ops = File_ops.lwt_file_ops


(** {2 Marshalling} *)

(** There are basically two types of marshalling one might
   consider. They differ in the behaviour when attempting to marshal
   to a point in a buffer where the remaining space is not
   sufficient. In the first "max-size-based" model, the marshaller
   knows the max size of an element, and the remaining space, and then
   can determine ahead of marshalling if the space is (potentially)
   not sufficient. In this case, the marshaller can abort early
   without attempting any modification of the buffer. However, it may
   be that the particular element would have actually fit the space,
   so the approach may be inefficient. Maybe the marshaller can
   calculate the marshalled size of each element (without actually
   performing the marshalling). As mentioned in the bin_prot
   documentation, this is rare because being able to calculate the
   marshalled size of an elt is often as difficult as trying to
   marshal the element (and failing when the space is
   insufficient). So if we assume the decision is based on the
   max-size, this approach may be inefficient. However, crucially, in
   the case that the marshalling is not attempted because of
   insufficient space, the buffer is unchanged.

This should be contrasted with the "always-try-to-write" approach,
   where the marshalling is attempted regardless of the available
   space. If an error occurs (lack of space), the marshalling is
   aborted. In this case, the buffer will almost certainly be altered
   from the write position onwards (because it is too costly to record
   what these bytes are and replace them on a failed marshal).

The B-tree datastructure arguably favours the "max-size-based" model,
   and so this is mostly the approach we take here.  *)

module Marshal_factory = Marshal_factory

type ('a,'buf) mshlr = ('a,'buf)Marshal_factory.mshlr = {
  max_elt_sz: int;
  mshl : 'a -> ('buf * int) -> 'buf * int;
  umshl: 'buf -> int -> 'a * int 
}

type ('k,'v,'buf) kv_mshlr = ('k,'v,'buf)Marshal_factory.kv_mshlr = {
  k_mshlr: ('k,'buf) mshlr;
  v_mshlr: ('v,'buf) mshlr;
}

let mshlrs = Marshal_factory.mshlrs

(** {2 Marshalling with bin-prot} *)

module Pvt_bin_prot_marshalling = Bin_prot_marshalling
type 'a bp_mshlr = 'a Pvt_bin_prot_marshalling.bp_mshlr
type 'a ba_mshlr = 'a Pvt_bin_prot_marshalling.ba_mshlr
let bp_mshlrs = Pvt_bin_prot_marshalling.bp_mshlrs


(** {2 Util: set_once...} *)

class ['a] set_once debug_name = object
  val mutable x = ((Obj.magic ()):'a)
  val mutable is_set = false
  method get =
    assert(
      if not is_set then 
        (Printf.printf "set_once, not initialized: %s\n%!" debug_name; true) 
      else true);
    assert(is_set);
    x
  method set y = 
    assert(not is_set);
    x<-y;
    is_set <- true
  method is_set = is_set
end



(** {2 Standard types and defns} *)

(**
{[
type ('r,'blk,'buf,'t) shared_ctxt = {
  r_cmp      : 'r -> 'r -> int;
  r_size     :int;
  buf_ops    :'buf Buf_ops.buf_ops;
  monad_ops  : 't monad_ops;
  async      : 't async;
  event_ops  : 't event_ops;
  blk_ops    : 'blk Blk_intf.blk_ops;
  blk_sz     : Blk_intf.blk_sz;
  buf_to_blk : 'buf -> 'blk;
  blk_to_buf : 'blk -> 'buf;
  buf_create : unit -> 'buf;
}

]}
*)

module Shared_ctxt = Shared_ctxt

let ctxt = Shared_ctxt.ctxt

(*
module Sh_ctxt = Sh_ctxt

module Sh_std_ctxt = Sh_ctxt.Std

class virtual ['r,'blk,'buf,'t] sh_ctxt = ['r,'blk,'buf,'t] Sh_ctxt.sh_ctxt

class sh_std_ctxt = Sh_std_ctxt.sh_std_ctxt
*)

(** {2 Testing} *)

module Test = Test

(** {2 Log} *)

module Log = Log


(** {2 Runtime config support} *)

(* FIXME move all runtime configs to use this interface *)
let runtime_config_factory = Runtime_config_factory.runtime_config_factory


(*

module Internal = struct
  module C = struct
    type config = {
      testing_enabled:bool
    } [@@deriving yojson]

    let default_config=Some{testing_enabled=false}
    let filename = "shared_config.json"
  end

  include Tjr_config.Make(C)
end

let testing_enabled = Internal.config.testing_enabled
let test = (if testing_enabled then (fun f -> f ()) [@inline] else fun f -> ()) 
let assert_ = (if testing_enabled then (fun f -> assert(f())) [@inline] else fun f -> ())

*)
