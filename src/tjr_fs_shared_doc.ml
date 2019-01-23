(** A summary of the main types provided by this package. *)

(** {2 Directory structure} 

{[
./src/b_global
     global.ml
./src/c_logger
     logger.ml
./src/c_test
     test.ml
./src/d_blocks_etc
     blk_dev_in_mem.ml
     blk_dev_on_fd.ml
     blk_dev_ops_type.ml
     blk_sz_type.ml
     block_ops.ml
     block_ops_type.ml
./src/d_fun_store_passing_monad
     store_passing.ml
./src/d_kv_op
     kv_op.ml
     kv_op_type.ml
./src/d_map_ops
     map_ops.ml
     map_ops_type.ml
./src/d_small_string
     small_string.ml
./src
     tjr_fs_shared_doc.ml
]}

*)

(** {2 Types } *)

include Blk_dev_ops_type

include Blk_sz_type

include Block_ops_type

type ('k,'v) op = ('k,'v) Kv_op_type.op = 
  | Insert of 'k * 'v
  | Delete of 'k

include Map_ops_type


type ss = Small_string.ss



(** {2 Implementations} *)

(** Note the following implementations:

- {!Blk_dev_in_mem}, {!Blk_dev_on_fd}, {!Block_ops.String_block_ops}
- {!Kv_op.default_kvop_map_ops}


*)
