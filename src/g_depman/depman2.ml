(** A dependency manager. At the moment this is a naive implementation
   but perhaps it should be replaced by ocamlgraph or similar.

Compared to depman, this uses dag terminology, and has extra
   functionality like get_leaves_under
*)

module Internal : 
sig
  type node
  type t
  val empty_depman : t
  val make_depman: deps:(int*int) list -> complete:int list -> t
  val add_edge : node -> node -> t -> t
  val gen_node : t -> node * t
  val mark_dead : node -> t -> t
  val is_dead : node -> t -> bool
  (* FIXME do we want to remove the complete nodes? Or somehow mark them as no longer part of the dependencies? yes *)
  (* val get_lt_set : ev_id -> t -> set *)
  val get_children : node -> t -> node list
  val get_descendants : node -> t -> node list
  val get_leaves_under: node -> t -> node list
  module Test : functor () -> sig  end
end
 = struct

  module S = Set_int

  module M = Map_int

  type node = { node: int }

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

  let to_node node = {node}

  let from_node {node} = node

  (* FIXME perhaps we also want to maintain a set of nodes which we are working with, rather than deriving this from the dependencies *)
  let gen_node : t -> node*t = 
    fun t -> 
    {node=t.min_free_id},{t with min_free_id=t.min_free_id+1}

  let mark_dead {node=e} t = 
    { t with
      complete=S.add e t.complete }

  (* FIXME perhaps we also want to indicate if this is a node we have actually seen or not *)
  let is_dead {node=e} t = 
    S.mem e t.complete


  let get_children_set {node=e} t = 
    M.find_opt e t.map |> (function
      | None -> S.empty
      | Some s -> s)

  (** NOTE returns a list *)
  let get_children e t = 
    get_children_set e t
    |> S.to_seq |> List.of_seq |> List.map to_node

  (** Get the children that are not dead *)
  let get_children e t =
    get_children e t |> List.filter (fun n -> not (is_dead n t))  

  let rec get_descendants_set e t = 
    get_children e t 
    |> (fun xs -> (S.of_list (List.map from_node xs))::List.map (fun e -> get_descendants_set e t) xs)  (* assumes acyclic *)
    (* now union everything together *)
    |> (fun xs -> (S.empty,xs))
    |> Iter.(iter_break (function
        | (s,[]) -> Break s
        | (s,x::xs) -> Cont (S.union x s, xs)))

  let get_descendants e t = get_descendants_set e t |> S.to_seq |> List.of_seq |> List.map to_node

  (** add an edge from e1 to e2 *)
  let add_edge: node -> node -> t -> t = 
    fun { node=e1 } { node=e2 } t -> 
    (* we want to check that it is not the case that e1 < e2 *)
    assert(not (S.mem e1 (get_descendants_set {node=e2} t)));
    { t with 
      map=(
        M.find_opt e1 t.map |> (function
          | None -> S.empty
          | Some s -> s)
        |> fun s ->
        S.add e2 s |> fun s ->
        M.add e1 s t.map |> fun m ->
        m          
      ) }


  let make_depman ~deps ~complete =
    (empty_depman,deps) 
    |> Iter.(iter_break (function
        | (t,[]) -> Break t
        | (t,(e1,e2)::deps) -> 
          add_edge {node=e1} {node=e2} t |> fun t -> 
          {t with min_free_id=max t.min_free_id (max (e1+1) (e2+1)) } |> fun t ->
          Cont(t,deps)))

  (** FIXME this is inefficient; maybe for each node we maintain an explicit set of leaves under that node, which gets updated on edge insertion *)
  let get_leaves_under n t = 
    get_descendants n t |> List.filter (fun n -> get_children n t = [])


  module Test() = struct
    (* FIXME make this module into a proper alcotest module *)
    
    let t = make_depman ~deps:[(2,0);(2,1); (3,2); (4,3) ] ~complete:[]

    let equal_list = Alcotest.(check (list int) "equal_list" )

    let _ = 
      assert(get_children {node=4} t = [{node=3}]); 
      equal_list (get_descendants {node=4} t |> List.map from_node) [0;1;2;3];
      equal_list (get_descendants {node=3} t |> List.map from_node) [0;1;2];
      equal_list (get_descendants {node=1} t |> List.map from_node) [];
      equal_list (get_leaves_under {node=4} t |> List.map from_node) [0;1]

    let t = mark_dead {node=1} t 

    let _ = 
      assert(get_children {node=4} t = [{node=3}]); 
      equal_list (get_descendants {node=4} t |> List.map from_node) [0;2;3];
      equal_list (get_descendants {node=3} t |> List.map from_node) [0;2];
      equal_list (get_descendants {node=1} t |> List.map from_node) [];
      equal_list (get_leaves_under {node=4} t |> List.map from_node) [0]
        

  end

end

include Internal

