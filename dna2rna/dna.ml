open Dnabuf;;
open Dna2rna;;

let dna = read_dna stdin
in execute dna empty_dna 1;;

(*
let buf = dna_from_base I
in write_dna (replace [T_Base F; T_Sub (0, 0); T_Abs (0)] [create "ICI"]) stdout;;
*)

(* write_dna (protect 80 buf empty_dna) stdout;; *)

(*
in let len = length buf
in let new_buf = subbuf (concat buf buf) (len / 2) (2 * len - len / 2)
in write_dna (skip (concat_base new_buf I) 10) stdout;;
*)
