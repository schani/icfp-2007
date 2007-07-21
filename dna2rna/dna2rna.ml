(* -*- fundamental -*- *)

open Dnabuf
open Printf
open Big_int

exception Hell;;

(****************************************************************************)

let finish rna =
  let file = open_out "/void/endo/endo.rna"
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

let rec get_nat (dna:dna) rna = 
  match consume dna with
    | None -> finish rna
    | Some(P,dna) -> (zero_big_int, dna)
    | Some(I,dna) 
    | Some(F,dna) -> let (n,dna) = get_nat dna rna in (add_big_int n n, dna)
    | Some(C,dna) -> let (n,dna) = get_nat dna rna in (add_big_int unit_big_int (add_big_int n n), dna)

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

(* val get_pattern: dna -> rna -> int -> pattern -> (dna * dna * pattern) *)
let rec get_pattern dna rna lvl p_rev =
  match consume dna with
    | None -> finish rna
    | Some(C,dna) -> get_pattern dna rna lvl (P_Base(I)::p_rev)
    | Some(F,dna) -> get_pattern dna rna lvl (P_Base(C)::p_rev)
    | Some(P,dna) -> get_pattern dna rna lvl (P_Base(F)::p_rev)
    | Some(I,dna) ->
	(match consume dna with
	   | None -> finish rna
	   | Some(C,dna) -> get_pattern dna rna lvl (P_Base(P)::p_rev)
	   | Some(P,dna) -> 
	       let (n, dna) = get_nat dna rna
		 in get_pattern dna rna lvl (P_Skip(n)::p_rev)
	   | Some(F,dna) ->
	       let dna = skip dna 1 in 		(* three bases consumed *)
	       let (s, dna) = get_consts dna
		 in get_pattern dna rna lvl (P_Search(s)::p_rev)
	   | Some(I,dna) ->
	       (match consume dna with
		  | None -> finish rna
		  | Some(P,dna) ->
		      get_pattern dna rna (lvl+1) (P_ParenL::p_rev)
		  | Some(C,dna) | Some(F,dna) ->
		      if lvl = 0 then
			(dna, rna, List.rev p_rev)
		      else
			get_pattern dna rna (lvl-1) (P_ParenR::p_rev)
		  | Some(I,dna) -> 
		      let first7 = subbuf dna 0 7 in 
			get_pattern (skip dna 7) (Rna.concat_rna rna first7) lvl p_rev
	       )
	)

(****************************************************************************)

(* val get_template: dna -> rna -> template -> (dna * dna * template) *)
let rec get_template dna rna t_rev =
  match consume dna with
    | Some(C,dna) -> 
	get_template dna rna ((T_Base I)::t_rev)
    | Some(F,dna) -> 
	get_template dna rna ((T_Base C)::t_rev)
    | Some(P,dna) -> 
	get_template dna rna ((T_Base F)::t_rev)
    | Some(I,dna) ->
	(match consume dna with
	   | None -> finish rna
	   | Some(C,dna) -> 
	       get_template dna rna ((T_Base P)::t_rev)
	   | Some(F,dna) | Some(P,dna) -> 
	       let (l,dna) = get_nat dna rna in
	       let (n,dna) = get_nat dna rna in
	         get_template dna rna ((T_Sub(n,l))::t_rev)
	   | Some(I,dna) ->
	       (match consume dna with
		  | Some(C,dna)
		  | Some(F,dna) -> 
		      (dna,rna,List.rev t_rev)
		  | Some(P,dna) -> 
		      let (n,dna) = get_nat dna rna in 
			get_template dna rna ((T_Abs(n))::t_rev)
		  | Some(I,dna) -> 
		      let rna = Rna.concat_rna rna (subbuf dna 0 7) in
		      let dna = skip dna 7 in
			get_template dna rna t_rev
		  | None -> finish rna
	       )
	)
    | None ->
	finish rna

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
      _ -> empty_dna;;

let replace tpl e =
  let rec work tpl r =
    match tpl with
	[] -> r
      | (T_Base base) :: tpl -> work tpl (concat_base r base)
      | (T_Sub (n, l)) :: tpl -> work tpl (protect (int_of_big_int l) (env_nth e n) r)
      | (T_Abs n) :: tpl -> work tpl (asnat (length (env_nth e n)) r)
  in
    work tpl empty_dna;;

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

(* val build_env : pattern -> dna -> ((dna list), int, dna) option *)
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
	  let s1 = search s dna
	  (* and s2 = Dnabuf.search s dna *)
	  in (* assert (s1 = s2); *)
	    (match s1 with
		 Some n ->
		   let n = n + (length s)
		   in build pat (skip dna n) (i + n) c env
	       | None -> None)
      | P_ParenL :: pat ->
	  build pat dna i (i :: c) env
      | P_ParenR :: pat ->
	  build pat dna i (List.tl c) ((subbuf dna_orig (List.hd c) i) :: env)
  in
    build pat dna_orig 0 [] [];;

let print_env env =
  let rec print env i =
    match env with
	[] -> ()
      | d :: env ->
	  print_int i; print_string ": "; write_dna d stdout; print_newline ();
	  print env (i + 1)
  in
    print env 0;;

let matchreplace pat tpl dna =
  match build_env pat dna with
      Some (env, i, dna) ->
	(print_string "got match:\n";
	 (* print_env env; *)
	 concat (replace tpl env) dna)
    | None ->
	(print_string "no match\n";
	 dna);;

(****************************************************************************)

let rec execute dna rna i =
  let the_dna = ref dna
  and the_rna = ref rna
  and i = ref i
  in while true do
      (* print_string "dna: "; write_dna !the_dna stdout; print_newline (); *)
      let (dna, rna, pat) = get_pattern !the_dna !the_rna 0 []
      in print_string "pattern: "; print_pattern pat; print_newline ();
	(* print_string "rna: "; write_dna rna stdout; print_newline (); *)
	let (dna, rna, tpl) = get_template dna rna []
	in print_string "template: "; print_template tpl; print_newline ();
	  (* print_string "concat same: "; print_int !num_concat_same; print_newline (); *)
	  (* print_string "rna: "; write_dna rna stdout; print_newline (); *)
	  if (!i mod 100000) = 99999 then
	    (let filename = sprintf "/void/endo/endo.%d.rna2" (!i / 100000)
	     in let file = open_out filename
	     in Rna.write_rna rna file; close_out file);
	  let dna = matchreplace pat tpl dna
	  in let dna = if (!i mod 2000) = 1999 then (flatten dna) else dna
	  (* in let rna = if (!i mod 2000) = 1999 then (flatten rna) else rna *)
	  in (* visualize dna; print_newline (); *)
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
