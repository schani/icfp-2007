open Dnabuf;;
open Dna2rna;;

let dna = read_dna stdin
in execute dna Rna.empty_rna 1;;

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

(*
let dna = read_dna stdin
in let dna = matchreplace [P_Skip 0; P_ParenL; P_Skip 4536645; P_ParenL; P_Skip 800; P_ParenR; P_Skip 2971964; P_ParenR]
    [T_Sub (0, 0); T_Sub (1, 0); T_Base I; T_Base C; T_Base F; T_Base P]
    dna
in write_dna dna stdout;;
*)
