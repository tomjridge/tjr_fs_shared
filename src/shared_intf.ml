(** {2 A record for the pair of an initial state and related operations} *)

type ('a,'b) initial_state_and_ops = {initial_state:'a; ops:'b}

type ba_buf = 
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t



(** {2 Marshalling and unmarshalling.}  *)

module Off_ = struct
  type off = int
end
open Off_

type ('a,'buf) mshl = {
  marshal   : 'a -> 'buf*off -> 'buf*off;
  unmarshal : 'buf -> off -> 'a * off;
  max_sz    : int; 
}
(** max_sz: maximum size of a marshalled elt *)


