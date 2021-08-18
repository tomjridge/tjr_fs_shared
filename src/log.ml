(** NOTE These functions were in Tjr_lib.Log *)

let log_now s = print_endline s

(** Display errors immediately *)
let log_error (s:string) = print_endline s

(** config: print_logs_at_exit *)
let print_logs_at_exit = ref true

(** log_lazy is used to store the lazy log msgs, and if there was a
   process exit (eg from an exception), the last 10 or so should be
   displayed. This prevents the output from being swamped by log
   messages but still gives good information for debugging an
   exception. *)
let log_lazy =
  let open (struct
    type state = { 
      max            : int; (* max thunks to save *)
      mutable thunks : (unit -> string) list; 
      mutable len    : int;
    }

    let empty () = {
        max=10;
        thunks=[];
        len=0;
      }

    let s = ref (empty (), empty ())
      
    let add' f =
      let (xs,_) = !s in
      xs.thunks <- f::xs.thunks;
      xs.len <- xs.len +1
    
    let add f =
      let (xs,_) = !s in
      match xs.len >= xs.max with
      | true -> 
        s:= (empty (),xs);
        add' f
      | false -> 
        add' f
      
    let print_thunks () = 
      let (xs,ys) = !s in
      let zs = List.append (List.rev ys.thunks) (List.rev xs.thunks) in
      zs |> List.iter (fun f -> print_endline (f()))

    let _ : unit = 
      Stdlib.at_exit (fun _ -> if !print_logs_at_exit then print_thunks () else ())

  end)
  in    
  fun f -> add f

