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


module Root_block = Root_block

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

module Write_back_cache = Write_back_cache

module Write_back_cache_v2_DONT_USE = Write_back_cache_v2




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

(** {2 Std types} *)

module Std_types = Std_types

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
