(** A dependency manager. At the moment this is a naive implementation
   but perhaps it should be replaced by ocamlgraph or similar. 

FIXME perhaps prefer pred rather than lt
*)

module Internal : 
sig
  type ev_id 
  type t
  val empty_depman : t
  val make_depman: deps:(int*int) list -> complete:int list -> t
  val add_lt : ev_id -> ev_id -> t -> t
  val gen_ev_id : t -> ev_id * t
  val note_complete : ev_id -> t -> t
  val is_complete : ev_id -> t -> bool
  (* val get_lt_set : ev_id -> t -> set *)
  val get_lt : ev_id -> t -> ev_id list
  val get_all_lt : ev_id -> t -> ev_id list
  module Test : functor () -> sig  end
end
 = struct

  module S = Set_int

  module M = Map_int

  type ev_id = { ev_id: int }

  type set = S.t

  type t = { 
    map         : set M.t;  (* predecessors of a given event *)
    min_free_id : int;
    complete    : S.t  (* probably not needed *)
  }


  let empty_depman = {
    map=M.empty;
    min_free_id=0;
    complete=S.empty
  }

  let to_ev_id ev_id = {ev_id}

  let from_ev_id {ev_id} = ev_id

  let gen_ev_id : t -> ev_id*t = 
    fun t -> 
    {ev_id=t.min_free_id},{t with min_free_id=t.min_free_id+1}

  let note_complete {ev_id=e} t = 
    { t with
      complete=S.add e t.complete }

  let is_complete {ev_id=e} t = 
    S.mem e t.complete


  let get_lt_set {ev_id=e} t = 
    M.find_opt e t.map |> (function
      | None -> S.empty
      | Some s -> s)

  (** NOTE returns a list *)
  let get_lt e t = 
    get_lt_set e t
    |> S.to_seq |> List.of_seq |> List.map to_ev_id

  
  let rec get_all_lt_set e t = 
    get_lt e t 
    |> (fun xs -> (S.of_list (List.map from_ev_id xs))::List.map (fun e -> get_all_lt_set e t) xs)  (* assumes acyclic *)
    (* now union everything together *)
    |> (fun xs -> (S.empty,xs))
    |> Iter.(iter_break (function
        | (s,[]) -> Break s
        | (s,x::xs) -> Cont (S.union x s, xs)))

  let get_all_lt e t = get_all_lt_set e t |> S.to_seq |> List.of_seq |> List.map to_ev_id

  let add_lt: ev_id -> ev_id -> t -> t = 
    fun { ev_id=e1 } { ev_id=e2 } t -> 
    (* we want to check that it is not the case that e2 < e1 *)
    assert(not (S.mem e2 (get_all_lt_set {ev_id=e1} t)));
    { t with 
      map=(
        M.find_opt e2 t.map |> (function
          | None -> S.empty
          | Some s -> s)
        |> fun s ->
        S.add e1 s |> fun s ->
        M.add e2 s t.map |> fun m ->
        m          
      ) }


  let make_depman ~deps ~complete =
    (empty_depman,deps) 
    |> Iter.(iter_break (function
        | (t,[]) -> Break t
        | (t,(e1,e2)::deps) -> 
          add_lt {ev_id=e1} {ev_id=e2} t |> fun t -> 
          {t with min_free_id=max t.min_free_id (max (e1+1) (e2+1)) } |> fun t ->
          Cont(t,deps)))


  module Test() = struct
    (* FIXME make this module into a proper alcotest module *)
    
    let t = make_depman ~deps:[(1,2); (2,3); (3,4) ] ~complete:[]

    let equal_list = Alcotest.(check (list int) "equal_list" )

    let _ = 
      assert(get_lt {ev_id=4} t = [{ev_id=3}]); 
      equal_list (get_all_lt {ev_id=4} t |> List.map from_ev_id) [1;2;3];
      equal_list (get_all_lt {ev_id=3} t |> List.map from_ev_id) [1;2];
      equal_list (get_all_lt {ev_id=1} t |> List.map from_ev_id) []
  end

end

include Internal

