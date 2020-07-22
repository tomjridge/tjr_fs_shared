(** Summary of main types for shared ctxt *)

(**
{[
type ('r,'blk,'buf,'t) shared_ctxt = {
  r_cmp      : 'r -> 'r -> int;
  r_size     :int;
  buf_ops    :'buf buf_ops;
  monad_ops  : 't monad_ops;
  async      : 't async;
  event_ops  : 't event_ops;
  blk_ops    : ('blk,'buf) blk_ops;
  blk_sz     : blk_sz;
  buf_to_blk : 'buf -> 'blk;
  blk_to_buf : 'blk -> 'buf;
  buf_create : unit -> 'buf;
}

]}
*)
