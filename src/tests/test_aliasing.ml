open OUnit2
open Core

let suite : test =
  "aliasing" >:::
  [ "aliasing1" >:: (fun ctxt -> skip_if true "unimplemented") ]
