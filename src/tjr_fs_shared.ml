(** Common types and defns for file system work.

A collection of the main types provided by this library. *)

(** {2 Intf types that don't belong anywhere else} *)

include Shared_intf


(** {2 Block-related types } *)

include Blk_intf


(** {2 Block-related implementations} *)

module Common_blk_ops = Common_blk_ops

module Blk_dev_in_mem = Blk_dev_in_mem

module Blk_dev_on_fd = Blk_dev_on_fd

module Common_blk_layers = Common_blk_layers

module Common_blk_stores = Common_blk_stores


(** {2 Functional-store-passing monad} *)

(* type fstore = Fstore_passing.fstore *)
type fstore_passing = Fstore_passing.fstore_passing
let fstore_passing_monad_ops = Fstore_passing.monad_ops
module Fstore_passing = Fstore_passing


(** {2 Kv ops} *)

(* NOTE we don't want to pollute the namespace with all the @@deriving
   functions so we dont include Kv_op_type directly *)
type ('k,'v) kvop = ('k,'v) Kv_op_type.kvop = 
  | Insert of 'k * 'v
  | Delete of 'k
    [@@deriving bin_io, yojson]

type ('k,'v) kvop_map = ('k,'v) Kv_op_type.kvop_map
module Kv_op = Kv_op


(** {2 (Shared) Map ops } *)

(** FIXME do we want to include this type at top level? *)
(* include Map_ops_type *)

(** Map operations find,ins,del, in; insert_all; make_insert_many FIXME? note may clash with other "map_ops" so we don't include at the top-level *)
(* module Map_ops = Map_ops *)
module Shared_map_ops = Shared_map_ops


(** {2 Small strings, leq 256 bytes} *)

type ss = Small_string.ss
module Small_string = Small_string


(** {2 Maps with key traversal: get_next_binding, get_prev_binding} *)

module Map_with_key_traversal = Map_with_key_traversal

(** {2 Simple sequence type} *)

module Tjr_seq = Tjr_seq


(** {2 Write back cache} *)

module Write_back_cache = Write_back_cache
include Write_back_cache


(** {2 Free list} *)

module Free_list = Free_list
type abstract_free_list = Free_list.abstract_free_list

(** NOTE don't expose free_list_ops record type since alloc and free are overloaded *)

let free_list_ops : (int,abstract_free_list)Free_list.free_list_ops = Free_list.free_list_ops
module Free_list_with_bin_prot = Free_list.Free_list_with_bin_prot


(** {2 File operations} *)

module File_ops = File_ops
include File_ops

(** {2 Dependency management} *)

(* module Depman = Depman *)
module Depman2 = Depman2


(** {2 Testing, controlled by optional config file "shared_config.json"} *)

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

