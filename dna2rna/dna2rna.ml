open Dnabuf
open Printf
open Big_int

let arg_output_filename = ref "/void/endo/endo.rna"
let arg_history_filename : string option ref = ref None
let arg_metahistory_path : string option ref = ref None

exception Hell;;

(****************************************************************************)

let finish rna =
  let file = open_out !arg_output_filename
  in Rna.write_rna rna file;
    close_out file;
    exit 0;;

type pattern_item = 
  | P_Base of base
  | P_Skip of big_int
  | P_Search of dna
  | P_ParenL
  | P_ParenR

type pattern = pattern_item list

let print_big_int n =
  print_string (string_of_big_int n)

let rec print_pattern = function
    [] -> ()
  | P_Base base :: pat ->
      print_char (base_to_char base); print_pattern pat
  | P_Skip n :: pat ->
      print_string "[!"; print_big_int n; print_string "]"; print_pattern pat
  | P_Search dna :: pat ->
      print_string "[?"; write_dna dna stdout; print_string "]"; print_pattern pat
  | P_ParenL :: pat ->
      print_string "("; print_pattern pat
  | P_ParenR :: pat ->
      print_string ")"; print_pattern pat

(* 
	d_str = dna string
	d_idx = current pos
	p_rev = pattern (in reversed order)
*)

type template_item =
  | T_Base of base
  | T_Sub of big_int * big_int
  | T_Abs of big_int

type template = template_item list

let rec print_template = function
    [] -> ()
  | T_Base base :: tpl ->
      print_char (base_to_char base); print_template tpl
  | T_Sub (n, l) :: tpl ->
      print_string "("; print_big_int n; print_string "_"; print_big_int l; print_string ")"; print_template tpl
  | T_Abs n :: tpl ->
      print_string "|"; print_big_int n; print_string "|"; print_template tpl

(****************************************************************************)

let rec get_nat dna =
  match consume dna with
    | None -> (None, dna)
    | Some(P,dna) -> (Some zero_big_int, dna)
    | Some(I,dna) 
    | Some(F,dna) ->
	 let (n,dna) = get_nat dna
	 in (match n with
		 Some n -> (Some (add_big_int n n), dna)
	       | None -> (None, dna))
    | Some(C,dna) ->
	 let (n,dna) = get_nat dna
	 in (match n with
		 Some n -> (Some (add_big_int unit_big_int (add_big_int n n)), dna)
	       | None -> (None, dna));;

let get_consts dna_orig =
  let rec work dna_orig s =
    match consume dna_orig with
      | None -> (s, dna_orig)
      | Some(C,dna) -> work dna (concat_base s I)
      | Some(F,dna) -> work dna (concat_base s C)
      | Some(P,dna) -> work dna (concat_base s F)
      | Some(I,dna) ->
	  (match consume dna with
	     | Some(C,dna) -> work dna (concat_base s P)
	     | _ -> (s, dna_orig))
  in
    work dna_orig empty_dna;;

(****************************************************************************)

(* val get_pattern: dna -> rna -> int -> pattern -> (dna * dna * pattern * int) *)
let get_pattern dna rna lvl p_rev =
  let rec work dna rna lvl p_rev rna_length =
    match consume dna with
      | None -> finish rna
      | Some(C,dna) -> work dna rna lvl (P_Base(I)::p_rev) rna_length
      | Some(F,dna) -> work dna rna lvl (P_Base(C)::p_rev) rna_length
      | Some(P,dna) -> work dna rna lvl (P_Base(F)::p_rev) rna_length
      | Some(I,dna) ->
	  (match consume dna with
	     | None -> finish rna
	     | Some(C,dna) -> work dna rna lvl (P_Base(P)::p_rev) rna_length
	     | Some(P,dna) -> 
		 let (n, dna) = get_nat dna
		 in (match n with
			 Some n -> work dna rna lvl (P_Skip(n)::p_rev) rna_length
		       | None -> finish rna)
	     | Some(F,dna) ->
		 let dna = skip dna 1 in 		(* three bases consumed *)
		 let (s, dna) = get_consts dna
		 in work dna rna lvl (P_Search(s)::p_rev) rna_length
	     | Some(I,dna) ->
		 (match consume dna with
		    | None -> finish rna
		    | Some(P,dna) ->
			work dna rna (lvl+1) (P_ParenL::p_rev) rna_length
		    | Some(C,dna) | Some(F,dna) ->
			if lvl = 0 then
			  (dna, rna, List.rev p_rev, rna_length)
			else
			  work dna rna (lvl-1) (P_ParenR::p_rev) rna_length
		    | Some(I,dna) -> 
			let first7 = subbuf dna 0 7 in 
			  work (skip dna 7) (Rna.concat_rna rna first7) lvl p_rev (rna_length + 1)
		 )
	  )
  in
    work dna rna lvl p_rev 0;;

(****************************************************************************)

(* val get_template: dna -> rna -> template -> (dna * dna * template * int) *)
let get_template dna rna t_rev =
  let rec work dna rna t_rev rna_length =
    match consume dna with
      | Some(C,dna) -> 
	  work dna rna ((T_Base I)::t_rev) rna_length
      | Some(F,dna) -> 
	  work dna rna ((T_Base C)::t_rev) rna_length
      | Some(P,dna) -> 
	  work dna rna ((T_Base F)::t_rev) rna_length
      | Some(I,dna) ->
	  (match consume dna with
	     | None -> finish rna
	     | Some(C,dna) -> 
		 work dna rna ((T_Base P)::t_rev) rna_length
	     | Some(F,dna) | Some(P,dna) -> 
		 let (l,dna) = get_nat dna
		 in let (n,dna) = get_nat dna
		 in (match (l, n) with
			 (Some l, Some n) -> work dna rna ((T_Sub(n,l))::t_rev) rna_length
		       | _ -> finish rna)
	     | Some(I,dna) ->
		 (match consume dna with
		    | Some(C,dna)
		    | Some(F,dna) -> 
			(dna,rna,List.rev t_rev, rna_length)
		    | Some(P,dna) -> 
			let (n,dna) = get_nat dna
			in (match n with
				Some n -> work dna rna ((T_Abs(n))::t_rev) rna_length
			      | _ -> finish rna)
		    | Some(I,dna) -> 
			let rna = Rna.concat_rna rna (subbuf dna 0 7) in
			let dna = skip dna 7 in
			  work dna rna t_rev (rna_length + 1)
		    | None -> finish rna
		 )
	  )
      | None ->
	  finish rna
  in
    work dna rna t_rev 0;;

(****************************************************************************)

let rec quote d prefix =
  match consume d with
      None -> prefix
    | Some (I, d) -> quote d (concat_base prefix C)
    | Some (C, d) -> quote d (concat_base prefix F)
    | Some (F, d) -> quote d (concat_base prefix P)
    | Some (P, d) -> quote d (concat_base (concat_base prefix I) C);;

let rec protect l d prefix =
  if l = 0 then
    (concat prefix d)
  else
    protect (l - 1) (quote d empty_dna) prefix;;

let rec asnat n prefix =
  if n = 0 then
    concat_base prefix P
  else if (n mod 2) = 0 then
    asnat (n / 2) (concat_base prefix I)
  else
    asnat (n / 2) (concat_base prefix C);;

(****************************************************************************)

let env_nth env n =
  try
    List.nth env (int_of_big_int n)
  with
      _ -> (empty_dna, -1);;

let replace tpl e =
  let rec work tpl r infos =
    match tpl with
	[] -> (r, infos)
      | (T_Base base) :: tpl ->
	  work tpl (concat_base r base) infos
      | (T_Sub (n, l)) :: tpl ->
	  let (group, group_start) = env_nth e n
	  in let new_r = (protect (int_of_big_int l) group r)
	  in work tpl new_r ((group_start, group_start + (length group), (length r), (length new_r)) :: infos)
      | (T_Abs n) :: tpl ->
	  let (group, group_start) = env_nth e n
	  in let new_r = (asnat (length group) r)
	  in work tpl new_r ((group_start, group_start + (length group), (length r), (length new_r)) :: infos)
  in
    work tpl empty_dna [];;

(****************************************************************************)

let rec search pat dna =
  let pat_len = length pat
  in let rec matches pat dna =
      match ((consume pat), (consume dna)) with
	  (None, _) -> true
	| (Some (base1, pat), Some (base2, dna)) ->
	    if base1 = base2 then
	      matches pat dna
	    else
	      false
	| _ -> raise Hell
     and work dna i =
      if (length dna) < pat_len then
	None
      else if matches pat dna then
	Some i
      else 
	work (skip dna 1) (i + 1)
  in
    work dna 0;;

(* val build_env : pattern -> dna -> (((dna * int) list), int, dna) option *)
let build_env pat dna_orig =
  let rec build pat dna i c env =
    match pat with
	[] ->
	  (assert (c = []);
	   Some ((List.rev env), i, dna))
      | P_Base base :: pat ->
	  (match consume dna with
	       None -> None
	     | Some (b, dna) ->
		 if base = b then
		   build pat dna (i + 1) c env
		 else
		   None)
      | P_Skip n :: pat ->
	  if (not (is_int_big_int n)) || ((int_of_big_int n) > (length dna)) then
	    None
	  else
	    build pat (skip dna (int_of_big_int n)) (i + (int_of_big_int n)) c env
      | P_Search s :: pat ->
	  let (* s1 = search s dna
	  and *) s2 = search s dna
	  in (* assert (s1 = s2); *)
	    (match s2 with
		 Some n ->
		   let n = n + (length s)
		   in build pat (skip dna n) (i + n) c env
	       | None -> None)
      | P_ParenL :: pat ->
	  build pat dna i (i :: c) env
      | P_ParenR :: pat ->
	  build pat dna i (List.tl c) ((subbuf dna_orig (List.hd c) i, List.hd c) :: env)
  in
    build pat dna_orig 0 [] [];;

let print_env env =
  let rec print env i =
    match env with
	[] -> ()
      | (d, _) :: env ->
	  print_int i; print_string ": "; write_dna d stdout; print_newline ();
	  print env (i + 1)
  in
    print env 0;;

(* val matchreplace : pattern -> template -> dna -> (dna * (match_info option)) *)
let matchreplace pat tpl dna_orig =
  match build_env pat dna_orig with
      Some (env, i, dna) ->
	(print_string "got match:\n";
	 (* print_env env; *)
	 let (replacement, infos) = replace tpl env
	 in (concat replacement dna, Some ((length dna_orig) - (length dna), length replacement, infos)))
    | None ->
	(print_string "no match\n";
	 (dna_orig, None));;

(****************************************************************************)

let rec print_replacement_infos file = function
    [] -> ()
  | (match_start, match_stop, repl_start, repl_stop) :: infos ->
      fprintf file "\t%d %d %d %d\n" match_start match_stop repl_start repl_stop;
      print_replacement_infos file infos;;

let rec execute dna rna i =
  let the_dna = ref dna
  and the_rna = ref rna
  and i = ref i
  and history_file = open_out "/void/endo/history"
  in while true do
      (* print_string "dna: "; write_dna !the_dna stdout; print_newline (); *)
      let (dna, rna, pat, pattern_rnas) = get_pattern !the_dna !the_rna 0 []
      in let pattern_len = (length !the_dna) - (length dna)
      in print_string "pattern: "; print_pattern pat; print_newline ();
	(* print_string "rna: "; write_dna rna stdout; print_newline (); *)
	let (dna, rna, tpl, template_rnas) = get_template dna rna []
	in let template_len = (length !the_dna) - (length dna) - pattern_len
	in print_string "template: "; print_template tpl; print_newline ();
	  (* print_string "concat same: "; print_int !num_concat_same; print_newline (); *)
	  (* print_string "rna: "; write_dna rna stdout; print_newline (); *)
	  if (!i mod 100000) = 99999 then
	    (let filename = sprintf "/void/endo/endo.%d.rna2" (!i / 100000)
	     in let file = open_out filename
	     in Rna.write_rna rna file; close_out file);
	  let (dna, info) = matchreplace pat tpl dna
	  in fprintf history_file "%d %d %d %d " pattern_len template_len pattern_rnas template_rnas;
	    (match info with
		 Some (match_len, replacement_len, replacement_infos) ->
		   fprintf history_file "%d %d\n" match_len replacement_len;
		   print_replacement_infos history_file replacement_infos
	       | None -> output_string history_file "\n");
	    let dna = if (!i mod 2000) = 1999 then (flatten dna) else dna
	      (* in let rna = if (!i mod 2000) = 1999 then (flatten rna) else rna *)
	    in (* visualize dna; print_newline (); *)
	      if !i = 1 then
		(let oc = open_out "/tmp/after1.dna" in 
		   write_dna dna oc; close_out oc);
	      the_dna := dna;
	      the_rna := rna;
	      i := !i + 1
    done;;

(*
let rec execute_loop dna rna = 
  let (p,dna,rna) = pattern dna rna []
  in let (t,dna,rna) = template dna rna
  in let (dna,rna) = matchreplace p t dna rna in
    execute_loop dna rna

let execute () = 
  let dna = input_line stdin in
  let rna = Buffer.create 80 in
    execute_loop dna rna

*)

(*
let main () =
  execute ()

let _ = main ()
*)
