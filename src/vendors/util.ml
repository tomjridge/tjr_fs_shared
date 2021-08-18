include Iter_k

(** tail recursive [l..h-1] FIXME prefer Base.List.range? *)
let from_upto l h = 
  let rec f l sofar = 
    if l>=h then List.rev sofar else f (l+1) (l::sofar)
  in 
  f l [] 

let file_exists fn = 
  try ignore (Unix.stat fn : Unix.stats); true with _ -> false
