(** Command line utility to create a blk store in a given directory... *)
open Tjr_fs_shared

let print_usage () = {|
Usage:
  <command> create dir
  <command> init dir

Creates (if necessary) a blk store under directory dir. If init is true, 
the store is reset to empty.
|} |> print_endline

let monad_ops = Tjr_monad.imperative_monad_ops

let _ =
  match Sys.argv |> Array.to_list with
  | _::["create";dir] -> 
    Common_blk_stores.make_blk_store_from_directory
      ~monad_ops
      ~dir
      ~init:false
      |> fun _x -> 
      ()
  | _::["init";dir] -> 
    Common_blk_stores.make_blk_store_from_directory
      ~monad_ops
      ~dir
      ~init:true
      |> fun _x -> 
      ()      
  | _ -> print_usage()
