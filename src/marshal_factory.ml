(** Common marshallers *)
open Buf_ops
(* open Shared_intf *)

(** Generic marshaller type, assuming max_elt_sz is known (alternative
   is to marshal and check for end of buffer explicitly) *)
type ('a,'buf) mshlr = {
  max_elt_sz: int;
  mshl : 'a -> ('buf * int) -> 'buf * int;
  umshl: 'buf -> int -> 'a * int 
}

(** Functor to construct a marshaller from a type deriving bin_io *)
module Make_marshaller(X: sig 
    type t[@@deriving bin_io] 
    val max_elt_sz:int (* FIXME bin_io likely already knows this *)
  end) 
= 
struct
  open X
  let mshl (x:t) (buf,off) = 
    bin_write_t buf ~pos:off x |> fun off' -> 
    (buf,off')

  let umshl buf off = 
    let pos_ref = ref off in
    bin_read_t buf ~pos_ref |> fun r ->
    (r,!pos_ref)

  let mshlr = { max_elt_sz; mshl; umshl }
end

type arg = 
  | A1_int_option__ba_buf

type res =
  | R1 of (int option,ba_buf) mshlr

let make_1 = 
  let module Int_option = struct
    open Bin_prot.Std
    type t = int option [@@deriving bin_io]
    let max_elt_sz=10
  end
  in
  let module X = Make_marshaller(Int_option) in
  X.mshlr
  

(*
let make = function
  | A1_int_option__ba_buf -> 
    let module Int_option = struct
      open Bin_prot.Std
      type int_opt = int option [@@deriving bin_io]
    end
    in
    let module X = struct
      let max_elt_sz = 10

      let m_elt (x:int option) (buf,off) = 
        Int_option.bin_write_int_opt buf ~pos:off x |> fun off' -> 
        (buf,off')

      let u_elt buf off = 
        let pos_ref = ref off in
        Int_option.bin_read_int_opt buf ~pos_ref |> fun r ->
        (r,!pos_ref)

      let x = { max_elt_sz; m_elt; u_elt }
    end
    in
    R1 X.x

(** Convenience *)
let int_option_marshaller = make (A1_int_option__ba_buf) |> fun (R1 x) -> x
*)
