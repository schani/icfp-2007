open Dnabuf
open Dna2rna
open Zweiundvierzig
open Big_int
open Printf

exception Unknown_insn;;

let green_zone_start = 13615;;
let green_zone_length = 7509409;;

let rec data_template tpl =
  match tpl with
      [] -> true
    | T_Base _ :: tpl -> data_template tpl
    | _ -> false;;

let is_push_polygon_template tpl =
  let rec work tpl len =
    match tpl with
	T_Base I :: rest
      | T_Base C :: rest
      | T_Base P :: rest ->
	  work rest (len + 1)
      | T_Sub (subn, subl) :: [] ->
	  (let subn = int_of_big_int subn
	   and subl = int_of_big_int subl
	   in if (subn = 0) && (subl = 0) then
	       Some len
	     else
	       None)
      | _ ->
	  None
  in
    work tpl 0;;

let rec bases2str tpl acc = 
  match tpl with
    | [] -> acc
    | T_Base x :: tpl -> bases2str tpl (acc^(String.make 1 (base_to_char x)))
    | _ -> assert false

let print_offset_data mnem offset tpl = 
  let n,_,_,_ = get_answer offset in
  let str = (bases2str tpl "") in
    print_string mnem;
    print_string " "; 
    print_string n;
    print_string ", ";
    print_string str;
    print_newline ()
      
let print_write_green_data offset tpl =
  print_offset_data "wgz" offset tpl
;;

let print_write_blue_data offset tpl =
  print_offset_data "wbz" offset tpl
;;

let print_reserve_stack_space len =
  print_string "morestack ";
  print_int len;
  print_newline ();
;;

let unsome s def =
  match s with
      Some x -> x
    | _ -> def;;

let print_function_call offset len =
  let (name, _, pop, push) = get_answer offset
  in print_string "call ";
    print_string name;
    print_string ":";
    printf "#x%x  ; " len;
    (match pop with
	 Some pop -> print_int pop
       | None -> print_string "?");
    print_string " -> ";
    (match push with
	 Some push -> print_int push
       | None -> print_string "?");
    print_newline ();
    (unsome pop 0, unsome push 0)
;;

let print_function_return skip =
  print_string "ret ";
  print_int skip;
  print_newline ()
;;

let print_push_polygon len =
  print_string "pushpoly ";
  print_int len;
  print_newline ();;

(* val print_insn : pattern -> template -> int -> int -> (bool * int) *)
let print_insn pat tpl rest_length stack_space =
  let in_green_zone addr len =
    (let start = addr - rest_length
     and stop = addr + len - rest_length
     in
       (start >= 0) && (start <= green_zone_length) &&
	 (stop >= 0) && (stop <= green_zone_length))
  and in_my_stack_space addr len =
    (let start = addr - rest_length
     and stop = addr + len - rest_length
     in
       (start >= green_zone_length) && (start <= green_zone_length + stack_space) &&
	 (stop >= green_zone_length) && (stop <= green_zone_length + stack_space))
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
			 (true, stack_space))
		      else if in_my_stack_space skip1 skip2 then
			(print_write_blue_data (skip1 - rest_length - green_zone_length) rest;
			 (true, stack_space))
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
			   (true, stack_space + skip1))
			else
			  raise Unknown_insn
		    | _ -> raise Unknown_insn)
	     | _ ->
		 raise Unknown_insn)
      | [P_ParenL; P_ParenL; P_Skip skip1; P_ParenR; P_Skip skip2; P_ParenR; P_Skip skip3] ->
	  (let skip1 = int_of_big_int skip1
	   and skip2 = int_of_big_int skip2
	   and skip3 = int_of_big_int skip3
	   in match tpl with
	       T_Sub (subn, subl) :: rest ->
		 (match is_push_polygon_template rest with
		      Some polygon_len ->
			let subn = int_of_big_int subn
			and subl = int_of_big_int subl
			in if (subn = 1) && (subl = 0) &&
			    ((skip1 + skip2) = (rest_length + green_zone_length)) &&
			    (skip3 <= stack_space) && ((skip1 + polygon_len) = 24528) then
			      (print_push_polygon (polygon_len / 12);
			       (true, stack_space + skip1 - skip3 + polygon_len))
			  else
			    (printf "numerics %d %d %d\n" rest_length stack_space polygon_len;
			     raise Unknown_insn)
		    | None ->
			raise Unknown_insn)
	     | _ ->
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
			  ((List.length rest) = 48) then
			    (let (pop, push) = print_function_call skip2 skip3;
			     in (true, stack_space - pop + push)) (* FIXME: function can return value *)
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
               [T_Base I; T_Base I; T_Base P; T_Base I; T_Base P; T_Sub (subn1, subl1); T_Base I; T_Base I; T_Base P; T_Base I; T_Base P; T_Sub (subn2, subl2); T_Base I; T_Base I; T_Base C; T_Base I; T_Base I; T_Base C; T_Base I; T_Base I; T_Base C; T_Base I; T_Base P; T_Base P; T_Base P; T_Base I; T_Base P; T_Base P; T_Base C; T_Base P; T_Base I; T_Base I; T_Base C; T_Sub (subn3, subl3)] ->
		 (let subn1 = int_of_big_int subn1
		  and subl1 = int_of_big_int subl1
		  and subn2 = int_of_big_int subn2
		  and subl2 = int_of_big_int subl2
		  and subn3 = int_of_big_int subn3
		  and subl3 = int_of_big_int subl3
		  in match (subn1, subl1, subn2, subl2, subn3, subl3) with
		      (1, 0, 2, 0, 0, 0) ->
			if skip1 = rest_length && skip2 = green_zone_length && skip3 = 24 && skip4 = 24 then
			  (print_function_return skip1;
			   (false, stack_space))
			else
			  raise Unknown_insn
		    | _ -> raise Unknown_insn)
	     | _ ->
		 raise Unknown_insn)
      | _ ->
	  raise Unknown_insn;;

let rec print_rnas = function
    [] -> ()
  | rna :: rest ->
      print_string "rna "; print_string (Rna.rna2string rna); print_newline ();
      print_rnas rest;;

let disassemble dna offset len =
  let rec disasm dna stack_space =
    printf "#x%06x: " (offset + (length dna) - len);
    (let (dna, rna, pattern, _) = get_pattern dna []
     in print_rnas (List.rev rna);
       let (dna, rna, template, _) = get_template dna []
       in print_rnas (List.rev rna);
	 try
	   let (not_last, stack_space) = print_insn pattern template (length dna) stack_space
	   in if not not_last then
	       (if stack_space > 0 then
		  printf "; warning: stack space %d\n" stack_space)
	     else
	       disasm dna stack_space
	 with
	     Unknown_insn ->
	       printf "Unknown insn (rest len %d): pattern: " (length dna); print_pattern pattern; print_newline ();
	       print_string "template: "; print_template template; print_newline ();
	       ())
  in
    disasm dna 0;;

let green_fragment dna start len =
  subbuf dna (start + green_zone_start) (start + len + green_zone_start);;
