(** Common types and defns for file system work.

A collection of the main types provided by this library. *)


(** {2 A record for the pair of an initial state and related operations} *)

type ('a,'b) initial_state_and_ops = {initial_state:'a; ops:'b}


(** {2 Buffers} *)

include Buf_ops

module Buf_factory = Buf_factory
open Buf_factory
module Buf_as_bigarray = Buf_as_bigarray
module Buf_as_bytes = Buf_as_bytes

let ba_buf_ops = Buf_as_bigarray.ba_buf_ops

let by_buf_ops = Buf_as_bytes.by_buf_ops



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


(** {2 Kv ops} *)

module Kv_op = Kv_op

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

module Write_back_cache_v2 = Write_back_cache_v2




(** {2 File operations} *)

module File_ops = File_ops

let lwt_file_ops = File_ops.lwt_file_ops


(** {2 Marshalling} *)

module Marshal_factory = Marshal_factory

type ('a,'buf) mshlr = ('a,'buf)Marshal_factory.mshlr = {
  max_elt_sz: int;
  mshl : 'a -> ('buf * int) -> 'buf * int;
  umshl: 'buf -> int -> 'a * int 
}


(** {2 Testing} *)

module Test = Test

(** {2 Log} *)

module Log = Log


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
