(** A summary of the main types provided by this package. *)

(** {0 Types } *)

include Blk_dev_ops_type

include Blk_sz_type

include Block_ops_type

type ('k,'v) op = ('k,'v) Kv_op_type.op = 
  | Insert of 'k * 'v
  | Delete of 'k

include Map_ops_type


type ss = Small_string.ss



(** {0 Implementations} *)

(** Note the following implementations:

- {!Blk_dev_in_mem}, {!Blk_dev_on_fd}, {!Block_ops.make_string_block_ops}
- {!Kv_op.default_kvop_map_ops}


*)
