(** Common types and defns for file system work.

A collection of the main types provided by this library. *)

(** {2 Block-related types } *)

type blk_sz = Blk_sz.blk_sz
module Blk_sz = Blk_sz
let bsz_to_int = Blk_sz.to_int
let bsz_of_int = Blk_sz.of_int

include Block_ops_type

include Blk_dev_ops_type.Export


(** {2 Block-related implementations} *)

module String_block_ops = Block_ops.String_block_ops

module Blk_dev_in_mem = Blk_dev_in_mem
module Blk_dev_on_fd = Blk_dev_on_fd



(** {2 Functional-store-passing monad} *)

type fstore = Fstore_passing.fstore
type fstore_passing = Fstore_passing.fstore_passing
let fstore_passing_monad_ops = Fstore_passing.monad_ops
module Fstore_passing = Fstore_passing


(** {2 Kv ops} *)

type ('k,'v) op = ('k,'v) Kv_op_type.op = 
  | Insert of 'k * 'v
  | Delete of 'k

module Kv_op = Kv_op


(** {2 Map ops (from shared) } *)

(** FIXME do we want to include this type at top level? *)
(* include Map_ops_type *)

(** Map operations find,ins,del, in; insert_all; make_insert_many *)
module Shared_map_ops = Map_ops



(** {2 Small strings, leq 256 bytes} *)

type ss = Small_string.ss
module Small_string = Small_string


(** {2 Maps with key traversal: get_next_binding, get_prev_binding} *)

module Map_with_key_traversal = Map_with_key_traversal

