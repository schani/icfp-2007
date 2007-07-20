(* -*- fundamental -*- *)

open String
open Buffer
open Dnabuf

(****************************************************************************)

let finish rna =
  let _ = print_string rna
  in exit 0

type pattern_item = 
  | Base of base
  | Skip of int
  | Search of string
  | Paren_L
  | Paren_R

type pattern = pattern_item list

(* 
	d_str = dna string
	d_idx = current pos
	p_rev = pattern (in reversed order)
*)

type template_item =
  | Base of char
  | Sub of int * int
  | Abs of int
  
type templates = template_item list

(****************************************************************************)

let rec get_nat dna rna = 
  match consume dna with
    | None -> finish rna
    | Some(P,dna) -> (0,dna)
    | Some(I,dna) 
    | Some(F,dna) -> let (n,dna) = get_nat dna rna in (n*2,dna)
    | Some(C,dna) -> let (n,dna) = get_nat dna rna in (n*2+1,dna)

let rec get_consts dna = 
  match consume dna with
    | None -> ([],dna)
    | Some(C,dna) -> let (s,dna) = get_consts dna in ((I::s),dna)
    | Some(F,dna) -> let (s,dna) = get_consts dna in ((C::s),dna)
    | Some(P,dna) -> let (s,dna) = get_consts dna in ((F::s),dna)
    | Some(I,dna) ->
	(match consume dna with
	   | Some(C,dna) -> let (s,dna) = get_consts dna in ((P::s),dna)
	   | _ -> ([],dna))

(****************************************************************************)

let rec get_pattern dna rna lvl p_rev =
  match consume dna with
    | None -> finish rna
    | Some(C,dna) -> get_pattern dna rna lvl (Base(I)::p_rev)
    | Some(F,dna) -> get_pattern dna rna lvl (Base(C)::p_rev)
    | Some(P,dna) -> get_pattern dna rna lvl (Base(F)::p_rev)
    | Some(I,dna) ->
	  (match consume dna with
	     | Some(C,dna) -> get_pattern dna rna lvl (Base(P)::p_rev)
	     | Some(P,dna) -> 
		  let (n,d_idx) = get_nat dna rna
		  in get_pattern dna rna lvl (Skip(n)::p_rev)
	     | Some(F,dna) -> 
		  let (s,d_idx) = get_consts dna
		  in get_pattern dna rna lvl (Search(s)::p_rev)
	     | Some(I,dna) ->
		 (match consume dna with
		    | Some(P,dna) ->
			get_pattern dna rna (lvl+1) (Paren_L::p_rev)
		    | Some(C,dna) | Some(F,dna) ->
			if lvl = 0 then List.reverse p_rev
			else get_pattern dna rna (lvl-1) (Paren_R::p_rev)
		    | Some(I,dna) -> 
			let first7 = subbuf dna 0 7 in 
			get_pattern (skip dna 7) (concat rna first7) lvl p_rev
		 )
	     | _ -> finish rna
	  )
      | _ -> finish rna

(****************************************************************************)

let get_template dna rna t_rev =
  match dnabuf.consume dna with
    | Some('C',dna) -> get_template dna rna ((Base 'I')::t_rev)
    | Some('F',dna) -> get_template dna rna ((Base 'C')::t_rev)
    | Some('P',dna) -> get_template dna rna ((Base 'F')::t_rev)
    | Some('I',dna) ->
	(match dnabuf.consume dna with
	   | Some('C',dna) -> get_template dna rna ((Base 'P')::t_rev)
	   | Some('F',dna) | Some('P',dna) -> 
	       let (l,dna) = get_nat dna rna in
	       let (n,dna) = get_nat dna rna in
	         get_template dna rna ((Sub(l,n))::t_rev)

	   | Some('I',dna) ->
	       (match dnabuf.consume dna with
		  | Some('C',dna)
		  | Some('F',dna) -> (dna,rna,List.reverse t_rev)
		  | Some('P',dna) -> let (n,dna) = get_nat dna rna in get_template dna rna ((Abs(n))::t_rev)
		  | Some('I',dna) -> 
		      let rna = concat rna (subbuf dna 0 7) in
		      let dna = skip dna 7 in
			get_template dna rna t_rev
		  | None -> finish rna
	       )
	)
    | None ->
	finish rna

(****************************************************************************)

let rec execute_loop dna rna = 
  let (p,dna,rna) = pattern dna rna []
  in let (t,dna,rna) = template dna rna
  in let (dna,rna) = matchreplace p t dna rna in
    execute_loop dna rna

let execute () = 
  let dna = input_line stdin in
  let rna = Buffer.create 80 in
    execute_loop dna rna




let main () =
  execute ()

let _ = main ()