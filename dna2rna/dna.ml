open Dnabuf;;

(* val read_dna : in_channel -> dna *)
let read_dna file =
  create (input_line file);;
