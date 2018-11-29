
open Core

let union ~eq l1 l2 =
  List.filter ~f:(fun x -> List.exists ~f:(eq x) l2) l1 @ l2

let rec changeWhen ~p ~f l = match l with
| [] -> []
| x::xs -> (if p x then f x else x) :: changeWhen ~p:p ~f:f xs

