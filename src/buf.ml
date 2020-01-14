(** Type of buffer operations *)

let chr0 = Char.chr 0

type 'buf buf_ops = {
  create : int -> 'buf;  (** Assumed to be zero-ed *)
  get    : int -> 'buf -> char;
  len    : 'buf -> int;
}

module Buf_as_bigarray = struct

  type ba_buf = 
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type ba_buf_ops = ba_buf buf_ops

  let ba_buf_ops : ba_buf_ops = Bigstring.{
      create=(fun n -> Bigstring.make n chr0);
      get=(fun i b -> get b i);
      len=Bigstring.length;
    }


end
