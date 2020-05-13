(** Basic bin-prot marshalling, as values via first-order modules

Example: 

{[
  let int_mshlr : int bp_mshlr = Bin_prot.Std.(
    let module X = struct
      type t = int[@@deriving bin_io]
      let max_sz = 9
    end
    in
    (module X))
]}

 *)

module type BP_MSHLR = sig
  type t[@@deriving bin_io]
  val max_sz: int
end

type 'a bp_mshlr = (module BP_MSHLR with type t = 'a)

(** NOTE hidden defns of marshallers *)

(**/**)

let int_mshlr : int bp_mshlr = Bin_prot.Std.(
    let module X = struct
      type t = int[@@deriving bin_io]
      let max_sz = 9
    end
    in
    (module X))

open Str_256

let s256_mshlr : str_256 bp_mshlr = (
    let module X = struct
      type t = str_256[@@deriving bin_io]
      let max_sz = 3+256 
    end
    in
    (module X))

let r_mshlr : Sh_ctxt.Std.r bp_mshlr = (
    let module X = struct
      type t = Sh_ctxt.Std.r[@@deriving bin_io]
      let max_sz = 9
    end
    in
    (module X))

(**/**)

let bp_mshlrs = object
  method int_mshlr = int_mshlr
  method s256_mshlr = s256_mshlr
  method r_mshlr = r_mshlr
end
