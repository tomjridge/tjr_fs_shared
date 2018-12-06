(** Simple logging *)

let log_ops = 
  Global.register ~name:"log_ops" (Tjr_log.mk_log_ops ())
