(** {2 A record for the pair of an initial state and related operations} *)

type ('a,'b) initial_state_and_ops = {initial_state:'a; ops:'b}


(** {2 Buffers} *)

type ba_buf = 
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let chr0 = Char.chr 0

type 'buf buf_ops = {
  create : int -> 'buf;  (** Assumed to be zero-ed *)
  get    : int -> 'buf -> char;
  set    : int -> char -> 'buf -> 'buf;
  len    : 'buf -> int;
}





(** {2 Marshalling and unmarshalling.}  *)

type ('a,'buf) mshl = {
  marshal   : 'a -> 'buf*int -> 'buf*int;
  unmarshal : 'buf -> int -> 'a * int;
  max_sz    : int; 
}
(** 
- the int argument is an offset
- max_sz: maximum size of a marshalled elt *)





(** {2 File operations} *)

open Tjr_file
open Tjr_file.Filenames

(** This is minimal, so we can support blk-dev-on-fd initialization
   and finalization. pread and pwrite always use blk-sized amounts of
   data. read_blk and write_blk take a blk_index *)
type ('fd,'blk,'t) file_ops = {
  read_file            : fn -> (string,'t)m;
  write_string_to_file : fn:fn -> string -> (unit,'t)m;
  stat                 : fn -> (fds_t option,'t)m;
  fd_from_file         : fn:fn -> create:bool -> init:bool -> ('fd,'t)m;
  close                : 'fd -> (unit,'t)m;
  blk_sz               : int;
  read_blk             : 'fd -> int -> ('blk,'t)m;
  write_blk            : 'fd -> int -> 'blk -> (unit,'t)m;
}
