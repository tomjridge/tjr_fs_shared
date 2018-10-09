(** Concrete representations of map operations *)

module Insert_delete = struct

  type ('k,'v) op = Insert of 'k * 'v | Delete of 'k

end


(* NOTE we may want to have variants of this with further operations *)
