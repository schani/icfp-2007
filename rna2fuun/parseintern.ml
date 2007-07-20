(* Module Parserintern
 *
 * A parser for our internal RNA format
 *)

open Rna

let parse_line str =
  match str.[0] with
    | 'K' -> RI_Color (RGB black)
    | 'R' -> RI_Color (RGB  red)
    | 'G' -> RI_Color (RGB green)
    | 'Y' -> RI_Color (RGB yellow)
    | 'B' -> RI_Color (RGB blue)
    | 'M' -> RI_Color (RGB magenta)
    | 'C' -> RI_Color (RGB cyan)
    | 'W' -> RI_Color (RGB white)
    | 'T' -> RI_Color (Alpha transparent)
    | 'O' -> RI_Color (Alpha opaque)
    | 'e' -> RI_ClearBucket
    | '^' -> RI_Move
    | '<' -> RI_RotateCounterClockwise
    | '>' -> RI_RotateClockwise
    | '=' -> RI_Mark
    | '-' -> RI_Line
    | '!' -> RI_Fill
    | '+' -> RI_AddBitmap
    | '*' -> RI_Compose
    | '&' -> RI_Clip
    | _ -> assert false

let read_trace filename =
  let ich = open_in filename
  and data = ref []
  in
    try
      while true do
	data := (parse_line (input_line ich)) :: !data
      done;
      !data
    with
	End_of_file -> List.rev !data
