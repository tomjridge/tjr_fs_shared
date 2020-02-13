(* NOTE this is now superseded by marshal_factory

(** A version of marshaling which interacts well with binprot *)

(* FIXME this stuff is now in marshal_factory *)

type 'k bin_prot_info = {
  max_size   : int; (* max number of bytes to marshal a 'k *)
  bin_reader : 'k Bin_prot.Type_class.reader;
  bin_writer : 'k Bin_prot.Type_class.writer
}

open Bin_prot.Std

module Make(X: sig type t[@@deriving bin_io] val max_size:int end) = struct
  include X
  let bin_prot_info : t bin_prot_info = {
    max_size;
    bin_reader=X.bin_reader_t;
    bin_writer=X.bin_writer_t
  }
end

let int_bin_prot_info = {
  max_size   = Bin_prot.Size.Maximum.bin_size_int;
  bin_reader = bin_reader_int;
  bin_writer = bin_writer_int
}

let ss_bin_prot_info = Tjr_fs_shared.Small_string.{
  max_size   = 3+max_length;
  bin_reader = bin_reader_ss;
  bin_writer = bin_writer_ss
}


(** {2 A type for holding key/value reader writers} *)

type ('k,'v) reader_writers = {
  read_k  : 'k Bin_prot.Type_class.reader;
  write_k : 'k Bin_prot.Type_class.writer;
  read_v  : 'v Bin_prot.Type_class.reader;
  write_v : 'v Bin_prot.Type_class.writer;
}


(** Various reader/writers *)
module Common_reader_writers = struct
  let int_int = {
    read_k  = bin_reader_int;
    write_k = bin_writer_int;
    read_v  = bin_reader_int;
    write_v = bin_writer_int;
  }

  open Small_string
  let ss_ss = {
    read_k  = bin_reader_ss;
    write_k = bin_writer_ss;
    read_v  = bin_reader_ss;
    write_v = bin_writer_ss;
  }

  let ss_int = {
    read_k  = bin_reader_ss;
    write_k = bin_writer_ss;
    read_v  = bin_reader_int;
    write_v = bin_writer_int;
  }

  module Int_int2 = struct
    type t = int*int [@@deriving bin_io]
  end

  let int_int2 = Int_int2.{
      read_k  = bin_reader_int;
      write_k = bin_writer_int;
      read_v  = bin_reader_t;
      write_v = bin_writer_t;
    }
end
*)
