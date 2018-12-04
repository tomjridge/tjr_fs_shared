(** Keep track of state eg config, bool vars etc etc *)

let register ~(name:string) a = 
  Printf.printf "%s: registered value with name %s\n%!" __MODULE__ name;
  a

