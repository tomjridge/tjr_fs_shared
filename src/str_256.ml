module Internal : sig
  type str_256 = private string[@@deriving bin_io, yojson]

  val make: string -> str_256
end = struct
  open Bin_prot.Std
  type str_256 = string[@@deriving bin_io,yojson]

  let make (s:string) = 
    assert(String.length s <= 256);
    (s:str_256)
end

include Internal
