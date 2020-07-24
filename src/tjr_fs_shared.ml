(** Common types and defns for file system work.

A collection of the main types provided by this library. *)

include Summary


(** {2 Int-like types} *)

module Int_like = Int_like
(* don't include, since we are in the process of moving to these types *)


(** {2 Buffers} *)

module Buf_ops = Buf_ops
type 'buf buf_ops = 'buf Buf_ops.buf_ops

let chr0 = Buf_ops.chr0


(** NOTE lwt uses bytes eg for pread and pwrite; FUSE prefers
   bigarray; so we have two main buffer implementations, bytes and
   bigarray

    $(ABBREV("ba = bigarray")) 

*)

type ba_buf = Buf_ops.ba_buf

type ba_buf_ops = Buf_ops.ba_buf_ops

let ba_buf_ops = Buf_ops.buf_ops#ba

let by_buf_ops = Buf_ops.buf_ops#bytes


(** {2 Block identifiers} *)

module Blk_intf = Blk_intf

module Blk_id_as_int = Blk_intf.Blk_id_as_int


(** {2 Blocks} *)

module Blk_sz = Blk_intf.Blk_sz
type blk_sz = Blk_sz.blk_sz
let blk_sz_4096 = Blk_sz.blk_sz_4096

module Blk_ops = Blk_intf.Blk_ops
type ('blk,'buf) blk_ops = ('blk,'buf) Blk_ops.blk_ops

let blk_ops = Blk_impls.blk_ops


(** {2 Block devices} *)

module Blk_dev_ops = Blk_intf.Blk_dev_ops

type ('blk_id,'blk,'t) blk_dev_ops 
= ('blk_id,'blk,'t) Blk_dev_ops.blk_dev_ops
= {
    blk_sz     : blk_sz; 
    write      : blk_id:'blk_id -> blk:'blk -> (unit,'t) m;
    read       : blk_id:'blk_id -> ('blk,'t) m;
    write_many : ('blk_id*'blk)list -> (unit,'t) m  (* FIXME may want to make this a seq? *)
  }

let blk_devs = Blk_dev_impls.blk_devs


(** {2 Block allocation} *)

module Blk_allocator_ops = Blk_intf.Blk_allocator_ops

type ('blk_id,'t) blk_allocator_ops 
= ('blk_id,'t) Blk_allocator_ops.blk_allocator_ops 
= {
    blk_alloc : unit -> ('blk_id,'t) m; 
    blk_free  : 'blk_id -> (unit,'t) m;
  }



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


(** {2 Small strings, leq 256 bytes} *)

module Str_256 = Str_256
type str_256 = Str_256.str_256


(** {2 Write back cache} *)

module Write_back_cache_v1_DONT_USE = Write_back_cache

module Write_back_cache_v2_DONT_USE = Write_back_cache_v2

module Write_back_cache_v3 = Write_back_cache_v3

module Write_back_cache = Write_back_cache_v3

type wbc_params = Write_back_cache.wbc_params
type ('k,'v,'t) wbc_ops = ('k,'v,'t) Write_back_cache.wbc_ops
type ('k,'v,'t) wbc_ops_plus = ('k,'v,'t) Write_back_cache.wbc_ops_plus
type ('k,'v,'t) wbc_factory = ('k,'v,'t) Write_back_cache.wbc_factory


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


(*
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
*)


(** {2 Standard example types and defns} *)

module Shared_ctxt = Shared_ctxt

let ctxt = Shared_ctxt.ctxt


(** {2 Testing} *)

module Test = Test


(** {2 Log} *)

module Log = Log


(** {2 Runtime config support} *)

(* FIXME move all runtime configs to use this interface *)
let runtime_config_factory = Runtime_config_factory.runtime_config_factory
