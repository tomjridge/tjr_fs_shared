(** Support for testing. Testing can be enabled/disabled. *)


(* testing ---------------------------------------------------------- *)

let run_test : ((unit -> unit) -> unit) ref = 
  Global.register 
    ~name:(__MODULE__^".run_test ref")
    (ref (fun f -> f ()))

let test f = (!run_test) f
let enable () = run_test := fun f -> f()
let disable () = run_test := fun _f -> ()
