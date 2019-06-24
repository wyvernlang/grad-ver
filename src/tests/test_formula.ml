open OUnit2
open Core

let suite : test =
  "formula" >:::
  [ "formula1" >:: (fun ctx -> skip_if true "unimplemented") ]
