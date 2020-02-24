(** Internal root block operations *)
open Blk_intf
open Buf_factory.Buf_as_bigarray
open Tjr_monad.With_lwt

module Blk_id = Blk_id_as_int

module type RT_BLK = sig

  type blk_id = Blk_id.blk_id
  type blk = ba_buf
    
  (** a container for data etc: data*blk_id*... *)
  type rt_blk

  (** typically lwt *)
  type t = lwt

  (** the data that is stored in the rt_blk *)
  type data[@@deriving bin_io]

  (* NOTE the blk operations and the identity of the root blk are set
     at creation time *)

  val sync_to_disk: rt_blk -> (unit,t)m
  (* val sync_from_disk: unit -> (rt_blk,t)m *)

  (** typically blk_id is 0 *)
  val make: blk_dev_ops:(blk_id,blk,t)blk_dev_ops -> blk_id:blk_id -> (rt_blk,t)m
      
  val read_from_disk: blk_dev_ops:(blk_id,blk,t)blk_dev_ops -> blk_id:blk_id -> (data,t)m

  val write_to_disk: blk_dev_ops:(blk_id,blk,t)blk_dev_ops -> blk_id:blk_id -> data:data -> (unit,t)m

end

module Rt_blk = struct
  module Make(S:sig type data[@@deriving bin_io] end) = struct
    include S

    type blk_id = Blk_id_as_int.blk_id
    type blk = ba_buf

    (** typically lwt *)
    type t = lwt

    type rt_blk = {
      blk_dev_ops:(blk_id,blk,t)blk_dev_ops;
      blk_id:blk_id;
      data:data;
    }

    let make ~(blk_dev_ops:(blk_id,blk,t)blk_dev_ops) ~blk_id = 
      (* let blk_id = Blk_id_as_int.of_int 0 in *)
      blk_dev_ops.read ~blk_id >>= fun blk -> 
      let data = S.bin_read_data blk ~pos_ref:(ref 0) in
      return {
        blk_dev_ops;
        blk_id;
        data=data
      }

    let write_to_disk ~(blk_dev_ops:(blk_id,blk,t)blk_dev_ops) ~blk_id ~data = 
      let buf = ba_buf_ops.create 4096 in
      let _ : int = S.bin_write_data buf ~pos:0 data in
      blk_dev_ops.write ~blk_id:blk_id ~blk:buf >>= fun () -> 
      return ()

    let read_from_disk ~(blk_dev_ops:(blk_id,blk,t)blk_dev_ops) ~blk_id = 
      blk_dev_ops.read ~blk_id >>= fun blk -> 
      S.bin_read_data blk ~pos_ref:(ref 0) |> fun data -> 
      return data

    let sync_to_disk rt_blk = 
      write_to_disk ~blk_dev_ops:(rt_blk.blk_dev_ops) ~blk_id:(rt_blk.blk_id) ~data:rt_blk.data

  end
end
