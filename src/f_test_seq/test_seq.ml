(** An implementation of functional sequences; for testing *)

module Type = struct
  type ('a,'t) test_seq = {
    take: int -> 't -> 'a list;
    take_and_drop: int -> 't -> 'a list * 't;  (* if the list is empty, the seq should be empty *)
  }
end
include Type

(* implement a sequence from l to h by a pair (l,h) *)

let ( -- ) = fun l h ->
  let rec take n (l,h) =
    match n>0 && l < h with
    | true -> l::take (n-1) (l+1,h)
    | false -> []
  in
  let rec take_and_drop n (l,h) =
    take n (l,h) |> fun xs ->
    xs,(l+List.length xs,h)
  in
  { take; take_and_drop}

let rec map f {take;take_and_drop} = {
  take=(fun n t -> take n t |> List.map f);
  take_and_drop=(fun n t -> 
      take_and_drop n t |> fun (xs,t) -> 
      (List.map f xs),t)
}

let iter f {take_and_drop;_} t = 
  let rec g t = 
    take_and_drop 1 t |> function
    | [],_ -> ()
    | [x],t -> f x; g t
    | _ -> failwith __LOC__
  in
  g t

;;

(* FIXME add inline testing *)
