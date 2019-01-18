(** Concrete representations of map operations insert and delete *)

type ('k,'v) op = Insert of 'k * 'v | Delete of 'k

(* NOTE we may want to have variants of this with further operations *)
