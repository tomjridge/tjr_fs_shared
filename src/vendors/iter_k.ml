(** Essentially the Y combinator; useful for anonymous recursive
   functions. The k argument is the recursive callExample:

{[
  iter_k (fun ~k n -> 
      if n = 0 then 1 else n * k (n-1))

]}


 *)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x
