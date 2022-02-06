(* NOTE moved from minifs_intf *)

open Bin_prot.Std
type times = {
  atim:float; 
  (* ctim:unit;   *)
  (* by default, we don't use this, but return ctim as mtim since atim
     apparently doesn't affect atim*)
  mtim:float;
} [@@deriving bin_io,yojson]

let update_atim atim t = {t with atim}

let update_mtim mtim t = {t with mtim}   

