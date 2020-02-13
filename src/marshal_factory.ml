(** Common marshallers *)
open Buf_ops
open Blk_intf
open Str_256
(* open Shared_intf *)

(** Generic marshaller type, assuming max_elt_sz is known (alternative
   is to marshal and check for end of buffer explicitly) *)
type ('a,'buf) mshlr = {
  max_elt_sz: int;
  mshl : 'a -> ('buf * int) -> 'buf * int;
  umshl: 'buf -> int -> 'a * int 
}

(** A pair of marshallers, one for keys, one for values *)
type ('k,'v,'buf) kv_mshlr = {
  k_mshlr: ('k,'buf) mshlr;
  v_mshlr: ('v,'buf) mshlr;
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
  | A2_blk_id_opt__ba_buf
  | A3_int__ba_buf
  | A4_str_256__ba_buf
  | A5_int_int_kv_mshlr
  | A6_s256_int_kv_mshlr
  | A7_s256_s256_kv_mshlr

type res =
  | R1 of (int option,ba_buf) mshlr
  | R2 of (Blk_id_as_int.blk_id option,ba_buf) mshlr
  | R3 of (int,ba_buf) mshlr
  | R4 of (str_256,ba_buf) mshlr
  | R5 of (int,int,ba_buf) kv_mshlr
  | R6 of (str_256,int,ba_buf) kv_mshlr
  | R7 of (str_256,str_256,ba_buf) kv_mshlr

open Bin_prot.Std

let make_1 = 
  let module Int_option = struct
    type t = int option [@@deriving bin_io]
    let max_elt_sz=10
  end
  in
  let module X = Make_marshaller(Int_option) in
  X.mshlr  

let make_2 = 
  let int_opt_mshlr = make_1 in
  let max_blk_id_sz = int_opt_mshlr.max_elt_sz in
      
  let m_blk_id blk_id = 
    blk_id |> (function None -> None | Some x -> Some (Blk_id_as_int.to_int x))
    |> int_opt_mshlr.mshl
  in
  let u_blk_id buf off = 
    int_opt_mshlr.umshl buf off |> fun (x,off) -> 
    x |> (function None -> None | Some x -> Some(Blk_id_as_int.of_int x)) |> fun x -> 
    (x,off)
  in
  let blk_id_mshlr = { max_elt_sz=max_blk_id_sz; mshl=m_blk_id; umshl=u_blk_id } in
  blk_id_mshlr

let make_3 = 
  let module Y = struct
    type t = int[@@deriving bin_io]
    let max_elt_sz = 9
  end
  in
  let module X = Make_marshaller(Y) in
  X.mshlr

let int_mshlr = make_3

let make_4 = 
  let module Y = struct
    type t = Str_256.str_256[@@deriving bin_io]
    let bin_prot_header_sz = 3 (* FIXME NOTE that str_128 would only require 1 byte *)
    let max_elt_sz = 256 + bin_prot_header_sz
  end
  in
  let module X = Make_marshaller(Y) in
  X.mshlr

let str_256_mshlr = make_4

let make_5 = {
  k_mshlr=int_mshlr;
  v_mshlr=int_mshlr
}

let make_6 = {
  k_mshlr=str_256_mshlr;
  v_mshlr=int_mshlr;
}

let make_7 = {
  k_mshlr=str_256_mshlr;
  v_mshlr=str_256_mshlr;
}

  


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
