(** Type of buffer operations *)

let chr0 = Char.chr 0

type 'buf buf_ops = {
  create : int -> 'buf;  (** Assumed to be zero-ed *)
  get    : int -> 'buf -> char;
  set    : int -> char -> 'buf -> 'buf;
  len    : 'buf -> int;
}

module Buf_as_bigarray = struct

  type ba_buf = 
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type ba_buf_ops = ba_buf buf_ops

  let ba_buf_ops : ba_buf_ops = Bigstring.{
      create=(fun n -> make n chr0);
      get=(fun i b -> get b i);
      set=(fun i c b -> set b i c; b);
      len=length;
    }
end


module Buf_as_bytes = struct

  type by_buf = bytes

  type by_buf_ops = by_buf buf_ops

  let by_buf_ops : by_buf_ops = Bytes.{
      create=(fun n -> make n chr0);
      get=(fun i b -> get b i);
      set=(fun i c b -> set b i c; b);
      len=length
    }

end
