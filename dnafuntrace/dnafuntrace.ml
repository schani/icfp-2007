(* trace fun calls in *NA *)

open Dnabuf
open Printf

let greenzone_intro = "IFPICFPPCFFPP"

let whitespace_regexp = Str.regexp "[ \t]+"

let rec asnat n prefix =
  if n = 0 then
    concat_base prefix P
  else if (n mod 2) = 0 then
    asnat (n / 2) (concat_base prefix I)
  else
    asnat (n / 2) (concat_base prefix C);;

let parse_rna ich =
  let result = ref []
  in
    try
      while true do
	let str = input_line ich
	in
	  if str.[0] = '?' then
	    match Str.bounded_split whitespace_regexp
	      (Str.string_after str 1) 3 with
		| [funname ; "1" ; rnahex] ->
		    let green_offset = Scanf.sscanf rnahex "%x" (fun x -> x)
		    in let n = (funname, green_offset,
			       (dna_to_string (asnat green_offset empty_dna)))
		    in
			 if not (List.mem n !result) then
			   result := n :: !result
		| _ -> ()
      done;
      []
    with
	_ -> List.rev !result

let read_dna ich = 
  let file_len = in_channel_length ich
  in let buf = String.create file_len
  in
    really_input ich buf 0 file_len;
    buf

let dump_rnacall_annotated (funname, green_offset, asnat, offsets) =
  printf "%s\t0x%06x\t%s: " funname green_offset asnat;
  List.iter (fun (x,(y,z)) -> printf "0x%x[%s @ 0x%x], " x y z) offsets;
  print_string "\n"

let extract_green_zone s =
  let r = Str.regexp_string greenzone_intro
  in
    Str.string_after s ((Str.search_forward r s 0) - 1)


let rec get_lower_rnacall ?(result="",0) addr = function
    [] -> result
  | (n, x, _) :: xs when x >= addr -> result
  | (n, x, _) :: xs -> get_lower_rnacall ~result:(n,x) addr xs

let extract_offsets_of_green_zone rnacalls green_zone str =
  let str_withoutP = String.sub str 0 ((String.length str) - 1)
  in let result = ref []
  and r = Str.regexp (str_withoutP ^ "I*P")
  and i = ref 0
  in
    try
      while true do
	let pos = Str.search_forward r green_zone !i
	in let annotation = get_lower_rnacall pos rnacalls
	in
	  result := (pos, annotation)  :: !result;
	  i := pos + 1
      done;
      []
    with
	Not_found -> !result

let rnacall_cmp (_, i1, _) (_, i2, _) =
  i1 - i2

let _ =
  let rnacalls = List.sort rnacall_cmp (parse_rna (open_in Sys.argv.(1)))
  in let dna = read_dna (open_in Sys.argv.(2))
  in let green_zone = extract_green_zone dna
  in let rnacalls_annotated =
    List.map (fun (x,y,s) ->
      x,y,s,extract_offsets_of_green_zone rnacalls green_zone s) rnacalls
  in
    List.iter dump_rnacall_annotated rnacalls_annotated;
    printf " .. and the green zone as %i bytes\n" (String.length green_zone)
