(** Keep track of state eg config, bool vars etc etc *)

let debug_global = ref false

let register ~(name:string) a = 
  (if !debug_global then 
     Printf.printf "%s: registered value with name %s\n%!" __MODULE__ name
   else ());
  a

