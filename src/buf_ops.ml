(** Operations on buffers *)

(* FIXME probably get/set are not really used; remove them? *)
type 'buf buf_ops = {
  create : int -> 'buf;  (** Assumed to be zero-ed *)
  get    : int -> 'buf -> char;
  set    : int -> char -> 'buf -> 'buf;
  len    : 'buf -> int;
}

let chr0 = Char.chr 0

type ba_buf = 
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type ba_buf_ops = ba_buf buf_ops

type by_buf_ops = bytes buf_ops


