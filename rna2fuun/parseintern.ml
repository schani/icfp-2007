(* Module Parserintern
 *
 * A parser for our internal RNA format
 *)

open Rna

let parse_line str =
  match str.[0] with
    | 'K' -> RI_RGB black
    | 'R' -> RI_RGB red
    | 'G' -> RI_RGB green
    | 'Y' -> RI_RGB yellow
    | 'B' -> RI_RGB blue
    | 'M' -> RI_RGB magenta
    | 'C' -> RI_RGB cyan
    | 'W' -> RI_RGB white
    | 'T' -> RI_Alpha transparent
    | 'O' -> RI_Alpha opaque
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
    | '?' -> RI_Ignore
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
