(** Keep track of state eg config, bool vars etc etc *)

(** This flag, if true, forces names of global vars to be printed when
   registered *)
let debug_global = ref false

module Internal = struct

  let names = ref []

  let print_all_names () = 
    List.iter (fun name -> print_endline name) (!names)

end
open Internal

let register ~(name:string) a = 
  names:=name::!names;
  (if !debug_global then 
     Printf.printf "%s: registered value with name %s\n%!" __MODULE__ name
   else ());
  a

