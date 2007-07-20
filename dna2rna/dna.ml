open Dnabuf;;

let buf = read_dna stdin
in let len = length buf
in let new_buf = subbuf (concat buf buf) (len / 2) (2 * len - len / 2)
in write_dna (skip (concat_base new_buf I) 10) stdout;;
