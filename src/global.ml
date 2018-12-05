(** Keep track of state eg config, bool vars etc etc *)

(** This flag, if true, forces names of global vars to be printed when
   registered *)
let debug_global = ref false

let register ~(name:string) a = 
  (if !debug_global then 
     Printf.printf "%s: registered value with name %s\n%!" __MODULE__ name
   else ());
  a

