open Dnabuf

exception Unknown_insn;;

let green_zone_start = 0;;
let green_zone_length = 0;;

let rec data_template tpl =
  match tpl with
      [] -> true
    | P_Base _ :: tpl -> data_template tpl
    | _ -> false;;

let print_write_green_data offset tpl =
;;

let print_write_blue_data offset tpl =
;;

let print_reserve_stack_space len =
;;


(* val print_insn : pattern -> template -> int -> int -> int option *)
let print_insn pat tpl rest_length stack_space =
  let in_green_zone addr =
  in
    match pat with
	[P_ParenL; P_Skip skip1; P_ParenR; P_Skip skip2] ->
	  (let skip1 = int_of_big_int skip1
	   and skip2 = int_of_big_int skip2
	   in match tpl with
	       T_Sub (subn, subl) :: rest ->
		 let subn = int_of_big_int subn
		 and subl = int_of_big_int subl
		 in if (subn = 0) && (subl = 0) && (data_template rest) then
		     (assert ((List.length rest) = skip2);
		      if in_green_zone skip1 skip2 then
			(print_write_green_data (skip1 - rest_length) rest;
			 Some stack_space)
		      else if in_my_stack_space skip1 skip2 then
			(print_write_blue_data (skip1 - rest_length - green_zone_length) rest;
			 Some stack_space)
		      else
			raise Unknown_insn)
		   else
		     raise Unknown_insn
	     | _ ->
		 raise Unknown_insn)
      | [P_ParenL; P_ParenL; P_Skip skip1; P_ParenR; P_Skip skip2; P_ParenR] ->
	  (let skip1 = int_of_big_int skip1
	   and skip2 = int_of_big_int skip2
	   in match tpl with
	       [T_Sub (subn1, subl1); T_Sub (subn2, subl2)] ->
		 (let subn1 = int_of_big_int subn1
		  and subl1 = int_of_big_int subl1
		  and subn2 = int_of_big_int subn2
		  and subl2 = int_of_big_int subl2
		  in match (subn1, subl1, subn2, subl2) with
		      (1, 0, 0, 0) ->
			if skip2 = (green_zone_length + rest_length - skip1) then
			  (print_reserve_stack_space skip1;
			    Some (stack_space + skip1))
			else
			  raise Unknown_insn
		    | _ -> raise Unknown_insn)
	     | ->
		 raise Unknown_insn)
      | [P_Skip skip1; P_ParenL; P_Skip skip2; P_ParenL; P_Skip skip3; P_ParenR; P_Skip skip4; P_ParenR] ->
	  (let skip1 = int_of_big_int skip1
	   and skip2 = int_of_big_int skip2
	   and skip3 = int_of_big_int skip3
	   and skip4 = int_of_big_int skip4
	   in match tpl with
	       (T_Sub (subn1, subl1)) :: (T_Sub (subn2, subl2)) :: rest ->
		 (let subn1 = int_of_big_int subn1
		  and subl1 = int_of_big_int subl1
		  and subn2 = int_of_big_int subn2
		  and subl2 = int_of_big_int subl2
		  in match (subn1, subl1, subn2, subl2) with
		      (0, 0, 1, 0) ->
			if (skip1 = rest_length) && (skip2 + skip3 + skip4 = green_zone_length) &&
			  ((length rest) = 48) then
			    ((* function call *);
			      Some stack_space)	(* FIXME: function can return value *)
			else
			  raise Unknown_insn
		    | _ -> raise Unknown_insn)
	     | _ ->
		 raise Unknown_insn)
      | [P_Skip skip1; P_ParenL; P_Skip skip2; P_ParenR; P_ParenL; P_Skip skip3; P_ParenR; P_ParenL; P_Skip skip4; P_ParenR] ->
	  (let skip1 = int_of_big_int skip1
	   and skip2 = int_of_big_int skip2
	   and skip3 = int_of_big_int skip3
	   and skip4 = int_of_big_int skip4
	   in match tpl with
	       [T_Base I; T_Base I; T_Base P; T_Base I; T_Base P; P_Sub (subn1, subl1); T_Base I; T_Base I; T_Base P; T_Base I; T_Base P; P_Sub (subn2, subl2)  T_Base I; T_Base I; T_Base C; T_Base I; T_Base I; T_Base C; T_Base I; T_Base I; T_Base C; T_Base I; T_Base P; T_Base P; T_Base P; T_Base I; T_Base P; T_Base P; T_Base C; T_Base P; T_Base I; T_Base I; T_Base C; P_Sub (subn3, subl3)] ->
		 (let subn1 = int_of_big_int subn1
		  and subl1 = int_of_big_int subl1
		  and subn2 = int_of_big_int subn2
		  and subl2 = int_of_big_int subl2
		  and subn3 = int_of_big_int subn3
		  and subl3 = int_of_big_int subl3
		  in match (subn1, subl1, subn2, subl2, subn3, subl3) with
		      (1, 0, 2, 0, 0, 0) ->
			if skip1 = rest_length && skip2 = green_zone_length && skip3 = 24 && skip4 = 24 then
			  ((* function return *);
			    None)
			else
			  raise Unknown_insn
		    | -> raise Unknown_insn)
	     | _ ->
		 raise Unknown_insn)
      | _ ->
	  raise Unknown_insn;;

let disassemble dna =
  let rec disasm dna =
    let (dna, rna, pattern, _) = get_pattern dna []
    in output_rnas rna;
      let (dna, rna, template, _) = get_template dna []
      in output_rnas rna;
	output_insn pattern template (length dna);
	disasm dna;;
