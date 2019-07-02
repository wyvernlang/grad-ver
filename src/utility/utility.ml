open Core

exception Unimplemented

(*--------------------------------------------------------------------------------------------------------------------------*)
(* debug messages *)
(*--------------------------------------------------------------------------------------------------------------------------*)

let toggle_debug = true

let makeHeader focus = if focus then "[$]---" else "[$]"

let debug ?(focus=false) ?(hide=false) msg : unit =
  let header = makeHeader focus in
  if not hide then
    begin
      print_endline "";
      print_endline header;
      print_endline msg;
    end
  else ()

let debugList ?(focus=false) ?(hide=false) msgs : unit =
  let header = makeHeader focus in
  if not hide then
    begin
      print_endline "";
      print_endline header;
      List.iter msgs ~f:print_endline;
    end
  else ()
