(* -*- fundamental -*- *)

open Dnabuf

(****************************************************************************)

let finish rna =
(*  let _ = print_string rna in *)
  exit 0

type pattern_item = 
  | P_Base of base
  | P_Skip of int
  | P_Search of dna
  | P_ParenL
  | P_ParenR

type pattern = pattern_item list

(* 
	d_str = dna string
	d_idx = current pos
	p_rev = pattern (in reversed order)
*)

type template_item =
  | T_Base of base
  | T_Sub of int * int
  | T_Abs of int
  
type templates = template_item list

(****************************************************************************)

let rec get_nat (dna:dna) (rna:dna) = 
  match consume dna with
    | None -> finish rna
    | Some(P,dna) -> (0,dna)
    | Some(I,dna) 
    | Some(F,dna) -> let (n,dna) = get_nat dna rna in (n*2,dna)
    | Some(C,dna) -> let (n,dna) = get_nat dna rna in (n*2+1,dna)

let rec get_consts dna = 
  match consume dna with
    | None -> (create "",dna)
    | Some(C,dna) -> let (s,dna) = get_consts dna in (concat_base s I,dna)
    | Some(F,dna) -> let (s,dna) = get_consts dna in (concat_base s C,dna)
    | Some(P,dna) -> let (s,dna) = get_consts dna in (concat_base s F,dna)
    | Some(I,dna) ->
	(match consume dna with
	   | Some(C,dna) -> let (s,dna) = get_consts dna in (concat_base s P,dna)
	   | _ -> (create "",dna))

(****************************************************************************)

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
	       let (n,d_idx) = get_nat dna rna
		 in get_pattern dna rna lvl (P_Skip(n)::p_rev)
	   | Some(F,dna) ->
	       let dna = skip dna 1 in 		(* three bases consumed *)
	       let (s,d_idx) = get_consts dna
		 in get_pattern dna rna lvl (P_Search(s)::p_rev)
	   | Some(I,dna) ->
	       (match consume dna with
		  | None -> finish rna
		  | Some(P,dna) ->
		      get_pattern dna rna (lvl+1) (P_ParenL::p_rev)
		  | Some(C,dna) | Some(F,dna) ->
		      if lvl = 0 then List.rev p_rev
		      else get_pattern dna rna (lvl-1) (P_ParenR::p_rev)
		  | Some(I,dna) -> 
		      let first7 = subbuf dna 0 7 in 
			get_pattern (skip dna 7) (concat rna first7) lvl p_rev
	       )
	)

(****************************************************************************)

let rec get_template dna (rna:dna) t_rev =
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
	         get_template dna rna ((T_Sub(l,n))::t_rev)
	   | Some(I,dna) ->
	       (match consume dna with
		  | Some(C,dna)
		  | Some(F,dna) -> 
		      (dna,rna,List.rev t_rev)
		  | Some(P,dna) -> 
		      let (n,dna) = get_nat dna rna in 
			get_template dna rna ((T_Abs(n))::t_rev)
		  | Some(I,dna) -> 
		      let rna = concat rna (subbuf dna 0 7) in
		      let dna = skip dna 7 in
			get_template dna rna t_rev
		  | None -> finish rna
	       )
	)
    | None ->
	finish rna

(****************************************************************************)

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