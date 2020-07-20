(** Blk-related interfaces 

The main choices:

- blk implementation (string, bytes, buf/bigarray etc)
- blk_id (presumably int, but perhaps a pair of (dev,int) )
- blk_dev (in mem, on file, on dev etc)

NOTE this is included in the top-level package module
*)


(** *)
module Blk_sz : sig 
  type blk_sz[@@deriving bin_io]
  val of_int: int -> blk_sz
  val to_int: blk_sz -> int
    
  (* a block size of 4096 *)
  val blk_sz_4096 : blk_sz
end = struct
  open Bin_prot.Std
  type blk_sz = int[@@deriving bin_io]  (* in bytes *)
  let of_int x = x
  let to_int x = x
  let blk_sz_4096 = 4096
end

type blk_sz = Blk_sz.blk_sz

let blk_sz_4096 = Blk_sz.blk_sz_4096

(** This is a common instance of blk_id; we don't open it by default
   because we want most code to be independent of the exact repn. of
   blk_id NOTE do not open this module FIXME replace with [{ blk_id:'a}]
   and [{ blk_id:int}] record *)
module Blk_id_as_int : sig 
  type blk_id[@@deriving bin_io, yojson, sexp]
  val of_int: int -> blk_id
  val to_int: blk_id -> int
  val inc: blk_id -> blk_id
  val incr: blk_id ref -> unit
end = struct
  open Bin_prot.Std
  open Sexplib.Std
  type blk_id = int[@@deriving bin_io, yojson, sexp]
  let of_int x = x
  let to_int x = x
  let inc x = x+1
  let incr x = x:=!x+1
end

module Blk_ops = struct
  (** NOTE Conversion to a blk is expected (and the common instances
     do, indeed!) to pad if the string/bytes are not long enough.

     NOTE since blk_sz is expected to be fixed for a given blk type,
     we don't include as a parameter on the main interface methods

     FIXME really blk_ops need to support conversion to/from bufs, so
     blk_ops should be parameterized by two types
      
      Are these blk_ops at all useful? if not, remove
  *)
  type 'blk blk_ops = {
    blk_sz    : blk_sz; 

    of_string : string -> 'blk;
    to_string : 'blk -> string;

    of_bytes  : bytes -> 'blk;
    to_bytes  : 'blk -> bytes;
  }
end
include Blk_ops


module Blk_dev_ops = struct
  (** A block device: read and write blocks. *)
  type ('blk_id,'blk,'t) blk_dev_ops = {
    blk_sz     : blk_sz; 
    write      : blk_id:'blk_id -> blk:'blk -> (unit,'t) m;
    read       : blk_id:'blk_id -> ('blk,'t) m;
    write_many : ('blk_id*'blk)list -> (unit,'t) m  (* FIXME may want to make this a seq? *)
  }
end
include Blk_dev_ops


module Blk_allocator_ops = struct
  (** A type for managing the free space on the disk. *)

  (** NOTE we assume alloc never fails, or that error is handled
      elsewhere in the monad; fields were named alloc and free *)
  type ('blk_id,'t) blk_allocator_ops = {
    blk_alloc : unit -> ('blk_id,'t) m; 
    blk_free  : 'blk_id -> (unit,'t) m;
  }
end
include Blk_allocator_ops

let add_profiling ~monad_ops ~blk_dev_ops = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let 
    [r1; w1; w2 ] =
    ["r1" ; "w1"; "w2" ]
    |> List.map Tjr_profile.intern[@@warning "-8"]
  in
  let prf = Tjr_profile.make_profiler 
      ~print_header:(Printf.sprintf "blk profiler %s" __LOC__)
      ~print_at_exit:true () 
  in
  let { blk_sz; read; write; write_many } = blk_dev_ops in
  let read ~blk_id = 
    prf.mark r1;
    read ~blk_id >>= fun b ->
    prf.mark (-1*r1);
    return b
  in
  let write ~blk_id ~blk = 
    prf.mark w1;
    write ~blk_id ~blk >>= fun () ->
    prf.mark (-1*w1);
    return ()
  in
  let write_many ws = 
    prf.mark w2;
    write_many ws >>= fun () ->
    prf.mark (-1*w2);
    return ()
  in
  { blk_sz;read;write;write_many }


let add_debug (blk_dev_ops: _ blk_dev_ops) = 
  let module B = Blk_id_as_int in
  let read ~blk_id =
    Printf.printf "blk_dev_ops %s: read %d\n%!" __LOC__ (B.to_int blk_id);
    blk_dev_ops.read ~blk_id
  in
  let write ~blk_id ~blk = 
    Printf.printf "blk_dev_ops %s: write %d\n%!" __LOC__ (B.to_int blk_id);
    blk_dev_ops.write ~blk_id ~blk
  in
  { blk_dev_ops with read; write }


type ('blk_id,'blk,'t,'fd) blk_dev_impl = <
  add_debug : 
    ('blk_id,'blk,'t)blk_dev_ops -> 
    ('blk_id,'blk,'t)blk_dev_ops;

  add_profiling: 
    ('blk_id,'blk,'t)blk_dev_ops -> 
    ('blk_id,'blk,'t)blk_dev_ops;

  with_ : blk_sz:blk_sz -> <
(* FIXME filename -> fd should just be part of lwt helper module
      from_filename: fn:string -> create:bool -> init:bool ->
        <
          blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops;
          fd          : 'fd;
          sync        : unit -> (unit,'t)m;
          close       : unit -> (unit,'t)m;
        >;
*)
      from_fd: 'fd -> 
        <
          blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops;
          fd          : 'fd;
          sync        : unit -> (unit,'t)m;
          close       : unit -> (unit,'t)m;
        >;
    >
>    


(*
(* This is used for talks, to avoid explaining labelled args *)
module Internal_unlabelled_blk_dev_ops = struct
  type ('blk_id,'blk,'t) blk_dev_ops = {
    blk_sz: blk_sz; 
    write: 'blk_id -> 'blk -> (unit,'t) m;
    read: 'blk_id -> ('blk,'t) m;
  }
end
*)

(*
(* FIXME remove *)
(** A blk layer has blk_ops and blk_dev_ops FIXME remove this? *)
module Blk_layer = struct
  (** Keep 'dev abstract since we may need to stage the
     construction. *)
  type ('blk,'dev) blk_layer = {
    blk_ops: 'blk blk_ops;
    blk_dev_ops: 'dev
  }
end

(** NOTE to access the field names, open Blk_layer *)
type ('blk,'dev) blk_layer = ('blk,'dev) Blk_layer.blk_layer
*)


(*

(* FIXME sync, close? remove this; favour blkdev with flags to indicate which functionality is available *)
(** A block store is like a block layer, but also includes an allocator *)
module Blk_store = struct
  type ('blk_id,'blk,'sync,'close,'t) blk_store = {
    blk_ops           : 'blk blk_ops;
    blk_dev_ops       : ('blk_id,'blk,'t)blk_dev_ops;
    blk_allocator_ops : ('blk_id,'t) blk_allocator_ops;
  }
end

(** NOTE to access field names, open Blk_store *)
type ('blk_id,'blk,'sync,'close,'t) blk_store = ('blk_id,'blk,'sync,'close,'t) Blk_store.blk_store 

*)
