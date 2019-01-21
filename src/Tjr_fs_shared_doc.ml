(** A summary of the main types provided by this package. *)

include Blk_dev_ops_type

include Blk_sz_type

include Block_ops_type

type ('k,'v) op = ('k,'v) Kv_op_type.op = 
  | Insert of 'k * 'v
  | Delete of 'k

include Map_ops_type


type ss = Small_string.ss

