(** Common blk devs; don't open *)


let blk_devs = 
  let lwt_impl = Blk_dev_impl_lwt.lwt_impl in
  (* NOTE this is convenience *)
  let lwt_open_file ~fn ~create ~trunc = 
    let open With_lwt in
    let open Shared_ctxt in
    (* The following combines the fd interface with the blkdev intf *)
    lwt_file_ops#open_file ~fn ~create ~trunc >>= fun fd ->
    lwt_impl#with_ ~blk_sz |> fun o -> 
    o#from_fd fd |> fun bd ->
    return bd
  in
  object
    method lwt=lwt_impl
    method lwt_open_file=lwt_open_file
    method in_mem=Blk_dev_impl_in_mem.make_blk_dev_in_mem
  end

