
open Core

let union ~eq l1 l2 =
  List.filter ~f:(fun x -> List.exists ~f:(eq x) l2) l1 @ l2

let rec changeWhen ~p ~f = function
  | [] -> []
  | x::xs -> (if p x then f x else x) :: changeWhen ~p:p ~f:f xs

let rec updateWhen ~p ~f = function
  | [] -> None
  | x::xs ->
      match updateWhen ~p:p ~f:f xs with
      | None -> if p x then Some (f x :: xs) else None
      | Some xs' -> Some ((if p x then f x else x) :: xs')

let rec foldSuffixes ~f ~init = function
  | [] -> init
  | x::xs -> foldSuffixes ~f:f ~init:(f init x xs) xs

