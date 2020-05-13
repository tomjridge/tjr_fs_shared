(** Strings with max length 256 bytes; safe to open *)

module Internal : sig
  type str_256 = private string[@@deriving bin_io, yojson]

  (** NOTE can throw an exception *)
  val make: string -> str_256
end = struct
  open Bin_prot.Std
  type str_256 = string[@@deriving bin_io,yojson]

  let make (s:string) = 
    assert(String.length s <= 256);
    (s:str_256)
end

include Internal


let s256_to_string (s:str_256) = (s :> string)

let to_string = s256_to_string
