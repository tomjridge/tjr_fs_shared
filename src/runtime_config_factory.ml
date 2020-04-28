
module type T = sig
  type config[@@deriving yojson]
end

type 'a runtime_config = (module T with type config = 'a)

let make (type a) ~(default:a option) ~(filename:string) (tt:a runtime_config) : a Lazy.t = 
  let module T = (val tt) in
  let module S = struct
    include T
    let default_config=default
    let filename=filename
  end
  in
  let module C = Tjr_config.Make(S) in
  C.config

let runtime_config_factory = object
  method make = make
end


module Example() = struct
  (** Example of using the runtime config factory *)

  type config'=int[@@deriving yojson]

  module T = struct
    type config = config'[@@deriving yojson]
    let default = None
    let filename = "kv_config.json"
  end

  let runtime_config = T.(runtime_config_factory#make ~default ~filename (module T))

  let _ = runtime_config

  (* let rc : int runtime_config = (module T) *)
end
