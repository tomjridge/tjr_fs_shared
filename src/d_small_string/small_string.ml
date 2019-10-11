(** Strings with <= 256 bytes, used as keys in maps. 

Really only for testing.
 *)


module Internal_ss : sig
  (** The type of small strings *)
  type ss [@@deriving bin_io, yojson]


  (** For this type, max_length is 256 *)
  val max_length: int
  val to_string: ss -> string
  val of_string: string -> ss
  val compare: ss -> ss -> int
end = struct
  open Bin_prot.Std
  type ss = string [@@deriving bin_io, yojson]
  let max_length = 256 
  let to_string x = x
  let of_string x = 
    assert (String.length x <= max_length);
    x
  let compare: ss -> ss -> int = Pervasives.compare
end

include Internal_ss



(* FIXME split into type and impl? *)
