open Dnabuf

type rnabase = 
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Transparent
    | Opaque
    | ClearBucket
    | Move
    | CCW
    | CW
    | Mark
    | Line
    | Fill
    | AddBitmap
    | Compose
    | Clip
    | Dead of string
type rna = rnabase list

let arg_breakpoint : rnabase option ref = ref None
let do_break = ref false

let empty_rna = []

(* val dna2rna: dna -> rnabase *)
let dna2rna dna = 
  match dna_to_string dna with 
    | "PIPIIIC" -> Black
    | "PIPIIIP" -> Red
    | "PIPIICC" -> Green
    | "PIPIICF" -> Yellow
    | "PIPIICP" -> Blue
    | "PIPIIFC" -> Magenta
    | "PIPIIFF" -> Cyan
    | "PIPIIPC" -> White
    | "PIPIIPF" -> Transparent
    | "PIPIIPP" -> Opaque
    | "PIIPICP" -> ClearBucket
    | "PIIIIIP" -> Move
    | "PCCCCCP" -> CCW
    | "PFFFFFP" -> CW
    | "PCCIFFP" -> Mark
    | "PFFICCP" -> Line
    | "PIIPIIP" -> Fill 
    | "PCCPFFP" -> AddBitmap
    | "PFFPCCP" -> Compose
    | "PFFICCF" -> Clip
    | dead -> Dead(dead)
	
(* val rnabase2string: rnabase -> string *)
let rna2string = function
    | Black  -> "K"
    | Red    -> "R"
    | Green  -> "G"
    | Yellow -> "Y"
    | Blue   -> "B"
    | Magenta -> "M"
    | Cyan   -> "C"
    | White  -> "W"
    | Transparent -> "T"
    | Opaque -> "O"
    | ClearBucket -> "e"
    | Move   -> "^"
    | CCW    -> "<"
    | CW     -> ">"
    | Mark   -> "="
    | Line   -> "-"
    | Fill   -> "!"
    | AddBitmap -> "+"
    | Compose -> "*"
    | Clip    -> "&"
    | Dead(dead) -> "?"^dead

(* val concat_rna: rna -> dna *)
let concat_rna rna dna =
  let new_rna = dna2rna dna
  in (match !arg_breakpoint with
	  Some breakpoint_rna -> if breakpoint_rna = new_rna then do_break := true
	| _ -> ());
    print_string "RNA out: "; print_string (rna2string new_rna); print_newline ();
    new_rna::rna;;
  
let write_rna rev_rna oc = 
  List.iter
    (fun rna -> 
      output_string oc (rna2string rna); 
      output_string oc "\n") 
    (List.rev rev_rna)
