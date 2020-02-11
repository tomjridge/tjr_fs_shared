(** This includes a block store that resides in a directory on disk (for examples). *)
open Blk_intf
(* open Common_blk_layers *)


type ('a,'b) t = {
  data_fn            : string;
  blk_allocator_ops  : 'a;
  sync_blk_allocator : 'b;
}

(** dir is the name of a directory where files will be stored:
- index.json is the index file (should contain blk_sz etc)
- data.blks is the file containing the blks
- blk_allocator.json contains the blk allocator state

FIXME at the moment we don't do anything with index.json

*)
let make_blk_store_from_directory ~monad_ops ~dir ~init = 
  let module A = struct

    (* NOTE this is an internal type only exposed in the backing file *)
    type blk_allocator = {
      min_free_blk_id: int
    } [@@deriving yojson]

    let blk_allocator0 = {
      min_free_blk_id=0
    }

    let return = monad_ops.return

    (* let blk_ops = Common_blk_ops.string_blk_ops *)
    (* let blk_sz = blk_ops.blk_sz *)

(*
    let blk_layer = blk_layer_unix_string_fd ~blk_sz
                      
    let _ = blk_layer
    let blk_dev_ops ~fd = blk_layer.blk_dev_ops ~monad_ops ~fd
    let _ = blk_dev_ops
*)

    let dir_exists d = Tjr_file.stat d = Some D

    include struct
      open Tjr_file.Slash_operator
      (* let index_fn = dir/"index.json" *)
      let data_fn = dir/"data.blks"
      let blk_allocator_fn = dir/"blk_allocator.json"
    end


    (* create files IF NECESSARY *)
    let create_if_necessary () = 
      (* create dir *)
      let _ = match Tjr_file.stat dir with
        | None -> FileUtil.mkdir dir
        | Some D -> ()
        | _ -> 
          Printf.sprintf "%s: path %s exists but is not a dir" __LOC__ dir |> failwith
      in

      (* create data file *)
      let _ = 
        match Tjr_file.stat data_fn with
        | Some F -> ()
        | Some _ -> Printf.sprintf "%s: path %s exists but is not a file" __LOC__ data_fn |> failwith
        | None -> FileUtil.touch ~create:true data_fn
      in

      (* create blk_allocator file *)
      let _ = 
        match Tjr_file.stat blk_allocator_fn with
        | Some F -> ()
        | None -> 
          blk_allocator0 |> blk_allocator_to_yojson |> Yojson.Safe.pretty_to_string
          |> Tjr_file.write_string_to_file ~fn:blk_allocator_fn
        | _ -> 
          Printf.sprintf "%s: path %s exists but is not a file" __LOC__ blk_allocator_fn |> failwith
      in
      ()

    let _ = create_if_necessary ()


    (* assume dir exists, reset the allocator and data files *)
    let init_ () = 
      assert(dir_exists dir);

      let _ = 
        match Tjr_file.stat data_fn with
        | Some F -> Unix.truncate data_fn 0
        | Some _ -> Printf.sprintf "%s: path %s exists but is not a file" __LOC__ data_fn |> failwith
        | None -> FileUtil.touch ~create:true data_fn
      in
      
      let _ = 
        match Tjr_file.stat blk_allocator_fn with
        | Some F | None -> 
          blk_allocator0 |> blk_allocator_to_yojson |> Yojson.Safe.pretty_to_string
          |> Tjr_file.write_string_to_file ~fn:blk_allocator_fn
        | _ -> 
          Printf.sprintf "%s: path %s exists but is not a file" __LOC__ blk_allocator_fn |> failwith
      in
      ()
      
    let _ = if init then init_ ()


    (* what do we want to return? perhaps only the "sync blk allocator
       state" function together with the blk allocator? and the
       filename for the data file?  *)
                           
    let read_blk_allocator () =
      Tjr_file.read_file blk_allocator_fn |> Yojson.Safe.from_string |> blk_allocator_of_yojson
      |> function | Ok x -> x | Error e -> failwith e

    let r = ref (read_blk_allocator ())

    let write_blk_allocator () = 
      !r |> blk_allocator_to_yojson |> Yojson.Safe.pretty_to_string 
      |> Tjr_file.write_string_to_file ~fn:blk_allocator_fn

    let sync_blk_allocator () =
      write_blk_allocator () |> return

    let blk_allocator_ops = Blk_allocator_ops.{
        blk_alloc=(fun () -> 
          ((!r).min_free_blk_id) |> fun blk_id ->
          r:={min_free_blk_id=blk_id+1};
          Blk_id_as_int.of_int blk_id |> return);
        blk_free=(fun blk_id -> return ())  (* FIXME do nothing for free - this is only for demos *)
    }
    
  end
  in
  A.{data_fn;sync_blk_allocator; blk_allocator_ops}
