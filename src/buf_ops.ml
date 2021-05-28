(** Buffer operations including blit, moved from Tjr_btree.

Don't use this module directly; instead use buf_ops#string_ etc


NOTE this uses value passing; to check linear usage, use the "safe_"
   version which tracks liveness of each buffer

*)

(* $(FIXME("""check linearity for sub operations (and others?)""")) *)

open Int_like 

let chr0 = Char.chr 0


(** "mutable", fixed size buffers; it is an error to attempt to write
   beyond the end; prefer a functional repr. for now 

    NOTE we use the value-passing intf

    $(FIXME("change default buffer implementation to bytes or bigstring for production"))

*)
(* $(PIPE2SH("""sed -n '/type[ ].buf buf_ops = /,/^}/p' >GEN.buf_ops.ml_ """)) *)
type 'buf buf_ops = {
  buf_create         : int -> 'buf;
  buf_length         : 'buf -> int;
  buf_get            : int -> 'buf -> char;
  buf_to_string      : src:'buf -> off:offset -> len:len -> string; 
  to_string          : 'buf -> string;
  of_string          : string -> 'buf;
  to_bytes           : 'buf -> bytes;
  of_bytes           : bytes -> 'buf;
  of_ba              : Bigstring.t -> 'buf;

  buf_sub            : buf:'buf -> off:int -> len:int -> 'buf;
  (* NOTE buf_sub either shares the underlying buf (mutable buffers),
     or allocates a new buf (immutable buffers) *)

  blit               : src:'buf   -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
  blit_bytes_to_buf  : src:bytes  -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
  blit_string_to_buf : src:string -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
}

type ba_buf = 
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type ba_buf_ops = ba_buf buf_ops

type by_buf_ops = bytes buf_ops


module B_ = BytesLabels

module Check_blit(S:sig 
    type t1 
    type t2 
    val len1: t1 -> int 
    val len2: t2 -> int 
  end) = struct
  open S
  let check ~(src:t1) ~src_off ~src_len ~(dst:t2) ~dst_off =
    let {off=src_off},{len=src_len},{off=dst_off} = src_off,src_len,dst_off in    
    let len = src_len in
    assert(len>=0 && src_off>=0 && dst_off>=0);
    assert (src_off + len <= len1 src);
    assert (dst_off + len <= len2 dst);
    ()
end


module String_ = struct  
  let buf_create n = String.make n (Char.chr 0)

  let buf_length = String.length 

  let buf_get i buf = String.get buf i

  let buf_to_string ~src ~off:{off} ~len:{len} =
    String.sub src off len

  let to_string s = s 

  let of_string s = s 

  let of_bytes = Bytes.to_string

  let to_bytes = Bytes.of_string

  let of_ba ba = Bigstring.to_string ba

  open (Check_blit(struct 
          type t1=bytes type t2=string let len1=Bytes.length let len2=String.length end))

  (** NOTE convert dst to bytes; blit; convert back to string FIXME this is inefficient *)
  let blit_bytes_to_buf ~(src:bytes) ~src_off ~src_len ~(dst:string) ~dst_off =
    assert(check ~src ~src_off ~src_len ~dst ~dst_off; true);
    let {off=src_pos} = src_off in
    let {len} = src_len in
    let dst : bytes = dst |> B_.of_string in
    let {off=dst_pos} = dst_off in
    assert (src_pos + len < B_.length src);
    assert (dst_pos + len < B_.length dst);
    B_.blit ~src ~src_pos ~dst ~dst_pos ~len;
    B_.to_string dst

  let blit_string_to_buf ~src ~src_off ~src_len ~dst ~dst_off =
    blit_bytes_to_buf ~src:(B_.of_string src) ~src_off ~src_len ~dst ~dst_off

  let blit = blit_string_to_buf

  let buf_sub ~buf ~off ~len = 
    String.sub buf off len


  let buf_ops = 
    {buf_create;buf_length;buf_get;buf_to_string;of_string;to_string;of_bytes;to_bytes;of_ba;
     blit;blit_bytes_to_buf;blit_string_to_buf;buf_sub}

end


(** Abstract buffer type, with underlying string representation; FIXME
   move to mutable implementation for production *)
module Abstract_string : sig
  type t
  val buf_ops: t buf_ops 
end = struct
  type t = string
  include String_
end


module Bytes_ = struct

  let buf_create n = B_.make n (Char.chr 0)

  let buf_length = B_.length 

  let buf_get i buf = Bytes.get buf i

  let buf_to_string ~src ~off:{off} ~len:{len} =
    B_.sub_string src ~pos:off ~len

  let of_string s = B_.of_string s 

  let to_string s = B_.to_string s 

  let of_bytes = fun bs -> bs
    
  let to_bytes = fun bs -> bs

  let of_ba = fun ba -> Bigstring.to_bytes ba

  open (Check_blit(struct 
          type t1=bytes type t2=bytes let len1=Bytes.length let len2=Bytes.length end))

  let blit_bytes_to_buf ~(src:bytes) ~src_off ~src_len ~(dst:bytes) ~dst_off =
    assert(check ~src ~src_off ~src_len ~dst ~dst_off; true);
    let {off=src_pos} = src_off in
    let {len} = src_len in
    (* let dst = dst |> B_.of_string in *)
    let {off=dst_pos} = dst_off in
    assert (src_pos + len < B_.length src);
    assert (dst_pos + len < B_.length dst);
    B_.blit ~src ~src_pos ~dst ~dst_pos ~len;
    dst
  
  let blit_string_to_buf ~(src:string) ~src_off ~src_len ~(dst:bytes) ~dst_off =
    blit_bytes_to_buf ~src:(Bytes.unsafe_of_string src) ~src_off ~src_len ~dst ~dst_off
  
  let blit = blit_bytes_to_buf

  let buf_sub ~buf ~off ~len = 
    Bytes.sub buf off len

  let buf_ops = 
    {buf_create;buf_length;buf_get;buf_to_string;of_string;to_string;of_bytes;to_bytes;of_ba;
     blit;blit_bytes_to_buf;blit_string_to_buf;buf_sub}

end


(** Abstract buffer type, with underlying bytes representation *)
module Abstract_bytes : sig
  type t
  val buf_ops : t buf_ops
end = struct
  type t = bytes
  let buf_ops = Bytes_.buf_ops
end


(*
(* The idea is to use "buffer passing", but to mark old buffers as
   "not ok". Attempts to access "not ok" buffers results in a
   runtime error. In some sense, the reference to the bytes (via the
   record... but bytes is mutable anyway) is the owner of the bytes,
   until some modifcation takes place. *)
module Safe_bytes : sig
  type t
  val buf_ops: t buf_ops 
end = struct
  
  type t = {
    mutable ok:bool;
    bytes: bytes
  }

  include Bytes_

  (* We modify the operations to assert(ok) before each operation, and
     to return a new buffer at the end (and mark the old as not ok) *)

  let wrap buf f =
    assert(buf.ok);
    f () |> fun bytes -> 
    buf.ok <- false;
    { ok=true; bytes }

  let on_bytes f x = f x.bytes

  let of_bytes bytes = { ok=true;bytes}

  let buf_create len = buf_create len |> of_bytes

  let buf_length = on_bytes buf_length

  let buf_get i = on_bytes (buf_get i)

  let buf_to_string ~src ~off ~len = on_bytes (fun src -> buf_to_string ~src ~off ~len) src

  let to_string = on_bytes to_string

  let of_string s = of_string s |> of_bytes

  let blit_bytes_to_buf ~(src:bytes) ~src_off ~src_len ~(dst:t) ~dst_off = 
    wrap dst (fun () -> 
        blit_bytes_to_buf ~src ~src_off ~src_len ~dst:dst.bytes ~dst_off)

  let blit_string_to_buf ~(src:string) ~src_off ~src_len ~(dst:t) ~dst_off = 
    wrap dst (fun () -> 
        blit_string_to_buf ~src ~src_off ~src_len ~dst:dst.bytes ~dst_off)

  let blit ~(src:t) ~src_off ~src_len ~(dst:t) ~dst_off = 
    wrap dst (fun () -> 
        blit ~src:src.bytes ~src_off ~src_len ~dst:dst.bytes ~dst_off)

  let _ = blit

  let buf_sub ~buf ~off ~len = 
    
        buf_sub ~buf ~off ~len)
  
  let buf_ops = 
    {buf_create;buf_length;buf_get;buf_to_string;of_string;to_string;of_bytes;
     blit;blit_bytes_to_buf;blit_string_to_buf;}
end
*)

(** NOTE this does not check linearity; use something else for testing *)
module Bigstring_ (*: sig
  type t = Bigstring.t
  val buf_ops: t buf_ops
end*) = struct
  type t = Bigstring.t
  let buf_create = Bigstring.create

  let buf_length = Bigstring.size

  let buf_get i buf = Bigstring.get buf i

  let buf_to_string ~src ~off ~len =
      Bigstring.sub_string src off.off len.len

  let to_string = Bigstring.to_string

  let of_string = Bigstring.of_string

  let to_bytes = Bigstring.to_bytes

  let of_bytes = Bigstring.of_bytes

  let of_ba = fun x -> x

  open (Check_blit(struct 
          type t1=string type t2=t let len1=String.length let len2=Bigstring.length end))

  let blit_string_to_buf ~src ~src_off ~src_len ~dst ~dst_off =
    assert(check ~src ~src_off ~src_len ~dst ~dst_off; true);
    Bigstring.blit_of_string src src_off.off dst dst_off.off src_len.len;
    dst


  open (Check_blit(struct 
          type t1=bytes type t2=t let len1=Bytes.length let len2=Bigstring.length end))

  let blit_bytes_to_buf ~src ~src_off ~src_len ~dst ~dst_off =
    assert(check ~src ~src_off ~src_len ~dst ~dst_off; true);
    Bigstring.blit_of_bytes src src_off.off dst dst_off.off src_len.len;
    dst

  open (Check_blit(struct 
          type t1=t type t2=t let len1=Bigstring.length let len2=Bigstring.length end))

  let blit ~src ~src_off ~src_len ~dst ~dst_off =
    assert(check ~src ~src_off ~src_len ~dst ~dst_off; true);
    Bigstring.blit src src_off.off dst dst_off.off src_len.len;
    dst

  (* FIXME we need to mark the resulting buf read only (or the original) *)
  let buf_sub ~buf ~off ~len = 
    Bigstring.sub buf off len

  let buf_ops = 
    {buf_create;buf_length;buf_get;buf_to_string;of_string;to_string;of_bytes;to_bytes;of_ba;
     blit;blit_bytes_to_buf;blit_string_to_buf;buf_sub}

end

(** Export all implementations under object value buf_ops *)
let buf_ops = object
  method string          = String_.buf_ops
  method abstract_string = Abstract_string.buf_ops
  method bytes           = Bytes_.buf_ops
  method abstract_bytes  = Abstract_bytes.buf_ops
  (* method safe_bytes      = Safe_bytes.buf_ops *)
  method ba              = Bigstring_.buf_ops
end
