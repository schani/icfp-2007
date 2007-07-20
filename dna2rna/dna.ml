open Dnabuf;;

(* val read_dna : in_channel -> dna *)
let read_dna file =
    let buf = Buffer.create 1048576
    in (Buffer.add_channel buf file;
	create (Buffer.contents buf))
