(** Summary of main types *)

(**
{[
  type ('blk_id,'t) blk_allocator_ops = {
    blk_alloc : unit -> ('blk_id,'t) m; 
    blk_free  : 'blk_id -> (unit,'t) m;
  }

type ('blk_id,'blk,'t,'fd) blk_dev_impl = <
  add_debug : 
    ('blk_id,'blk,'t)blk_dev_ops -> 
    ('blk_id,'blk,'t)blk_dev_ops;

  add_profiling: 
    ('blk_id,'blk,'t)blk_dev_ops -> 
    ('blk_id,'blk,'t)blk_dev_ops;

  with_ : blk_sz:blk_sz -> <
      from_fd: 'fd -> 
        <
          blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops;
          fd          : 'fd;
          sync        : unit -> (unit,'t)m;
          close       : unit -> (unit,'t)m;
        >;
    >
>    

  type ('blk_id,'blk,'t) blk_dev_ops = {
    blk_sz     : blk_sz; 
    write      : blk_id:'blk_id -> blk:'blk -> (unit,'t) m;
    read       : blk_id:'blk_id -> ('blk,'t) m;
    write_many : ('blk_id*'blk)list -> (unit,'t) m  (* FIXME may want to make this a seq? *)
  }

  type ('blk,'buf) blk_ops = {
    blk_sz: blk_sz;
    blk_to_buf: 'blk -> 'buf;
    buf_to_blk: 'buf -> 'blk;
  }

type 'buf buf_ops = {
  buf_create         : int -> 'buf;
  buf_length         : 'buf -> int;
  buf_get            : int -> 'buf -> char;
  buf_to_string      : src:'buf -> off:offset -> len:len -> string; 
  to_string          : 'buf -> string;
  of_string          : string -> 'buf;
  of_bytes           : bytes -> 'buf;

  blit               : src:'buf   -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
  blit_bytes_to_buf  : src:bytes  -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
  blit_string_to_buf : src:string -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
}

type ('blk_id,'t) freelist_factory = <
  with_:
    fn:string -> 
    <

      create: min_free:'blk_id -> ( ('blk_id,'t)freelist_ops,'t)m;
      (** Create a new freelist *)

      restore: unit -> ( ('blk_id,'t)freelist_ops,'t)m;
      (** Restore from file *)

    >      
>

type ('blk_id,'t) freelist_ops = {
  alloc      : unit -> ('blk_id,'t)m;
  alloc_many : int -> ('blk_id list,'t)m;
  free       : 'blk_id -> (unit,'t)m;
  free_many  : 'blk_id list -> (unit,'t)m;
  sync       : unit -> (unit,'t)m;
  (** NOTE the freelist already ensures it is crash safe; this sync is
      really for tidy shutdown *)
}

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
