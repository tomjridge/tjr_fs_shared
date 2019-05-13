module Internal : sig 
  type blk_sz
  val of_int: int -> blk_sz
  val to_int: blk_sz -> int
end = struct
  type blk_sz = int  (* in bytes *)
  let of_int x = x
  let to_int x = x
end

include Internal

