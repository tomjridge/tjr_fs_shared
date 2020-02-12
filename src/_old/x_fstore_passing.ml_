(** A monad which passes a functional store. FIXME move elsewhere?
   NOTE used for testing dmap etc, but common in other repos *)

open Tjr_store

type fstore_passing = fstore State_passing.state_passing (* monad type is ('a,t) m *)

let monad_ops : fstore_passing monad_ops = 
  State_passing.monad_ops ()

let ( >>= ) = monad_ops.bind 
let return = monad_ops.return


(** Link to Tjr_monad.with_state. Given an fstore ref, convert to
   fstore_passing with_state *)

(** FIXME NOTE the following is correct providing there is no
   interleaving in the set_state impl; we only mutate the part of the
   state corresponding to the ref; FIXME we need to make clear which
   steps are atomic *)
let fstore_ref_to_with_state (r:'part_of_state ref_) = 
  let get_state () = sp_of_fun (fun t -> (t,t)) in
  let _ = get_state in
  let set_state s = sp_of_fun (fun _ -> ((),s)) in
  let with_state (type a)
      (f: state:'part_of_state -> 
       set_state:('part_of_state -> (unit,fstore_passing)m) -> 
       (a,fstore_passing) m)
    : (a,fstore_passing)m = 
    get_state () >>= fun fstore -> 
    fstore |> Tjr_store.get r |> fun part -> 
    f ~state:part
      ~set_state:(fun part -> get_state () >>= fun fstore -> 
        fstore |> Tjr_store.set r part |> fun fstore ->
        set_state fstore)        
  in
  { with_state }
    
