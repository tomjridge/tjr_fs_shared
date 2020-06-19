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

type ba_buf = Buf_ops.ba_buf
let ba_buf_ops = Buf_factory.Buf_as_bigarray.ba_buf_ops


(** $(ABBREV("BP is Bin_prot")) *)
module type BP_MSHLR = sig
  type t[@@deriving bin_io]
  val max_sz: int
end

type 'a bp_mshlr = (module BP_MSHLR with type t = 'a)


(** $(ABBREV("BA is Bigarray")) *)
module type BA_MSHLR = sig
  type t
  val unmarshal   : ba_buf -> t
  val marshal     : t -> ba_buf
end

type 'a ba_mshlr = (module BA_MSHLR with type t = 'a)

(* convert an 'a bp_mshlr into something that can convert to and from a ba_buf *)
let ba_mshlr (type t) ~(mshlr:t bp_mshlr) ~(buf_sz:int) : t ba_mshlr =
  let module A = struct
    include (val mshlr)
    let _ : unit = assert(buf_sz >= max_sz)
    let unmarshal = fun buf -> 
      let t = bin_read_t buf ~pos_ref:(ref 0) in
      t
    let marshal = fun t -> 
      let buf = ba_buf_ops.create buf_sz in (* NOTE not necessarily zeroed *)
      let n = bin_write_t buf ~pos:0 t in
      buf
  end
  in
  (module A)      


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

let r_mshlr : Shared_ctxt.r bp_mshlr = (
    let module X = struct
      type t = Shared_ctxt.r[@@deriving bin_io]
      let max_sz = 9
    end
    in
    (module X))

(**/**)

let bp_mshlrs = object
  method int_mshlr = int_mshlr
  method s256_mshlr = s256_mshlr
  method r_mshlr = r_mshlr
  method ba_mshlr : 'a. mshlr:'a bp_mshlr -> buf_sz:int -> 'a ba_mshlr = ba_mshlr
end
