(** Simple in-memory block device. *)

(* open Tjr_monad.Types *)
open Blk_dev_ops_type

(* type blk_dev_in_mem = Blk_dev_in_mem *)

(*
module Internal = struct
  type blk_id = int
  type blk = string
  type dev = (blk_id,blk) Tjr_polymap.t
end
*)
(* open Internal *)

(* open Tjr_map *)

(** NOTE blk_sz is not checked - any size blk can be used *)
let make ~monad_ops ~blk_sz ~with_state = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in   
  let with_state = with_state.with_state in
  let write ~(blk_id:'blk_id) ~(blk:'blk) =
    with_state (fun ~state:s ~set_state ->
        set_state (Tjr_poly_map.add blk_id blk s) >>= fun _ -> 
      return ())
  in
  let read ~blk_id =
    (* NOTE assume never try to access an uninitialized blk *)
    with_state (fun ~state:s ~set_state ->
        return (Tjr_poly_map.find blk_id s))
  in
  { blk_sz; read; write }
  
  
 
let _ = make
