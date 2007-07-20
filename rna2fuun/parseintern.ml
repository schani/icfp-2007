(* Module Parserintern
 *
 * A parser for our internal RNA format
 *)

open Rna2fuun

let parse_line str =
  match str.[0] with
    | 'K' :: _ -> RI_Color black
    | 'R' :: _ -> RI_Color red
    | 'G' :: _ -> RI_Color green
    | 'Y' :: _ -> RI_Color yellow
    | 'B' :: _ -> RI_Color blue
    | 'M' :: _ -> RI_Color magenta
    | 'C' :: _ -> RI_Color cyan
    | 'W' :: _ -> RI_Color white
    | 'T' :: _ -> RI_Color transparent
    | 'O' :: _ -> RI_Color opaque
    | 'e' :: _ -> RI_ClearBucket
    | '^' :: _ -> RI_Move
    | '<' :: _ -> RI_RotateCounterClockwise
    | '>' :: _ -> RI_RotateClockwise
    | '=' :: _ -> RI_Mark
    | '-' :: _ -> RI_Line
    | '!' :: _ -> RI_Fill
    | '+' :: _ -> RI_AddBitmap
    | '*' :: _ -> RI_Compose
    | '&' :: _ -> RI_Clip

let read_trace filename =
  let ich = open_in filename
  and data = ref []
  in
    try
      while true do
	data := (parse_line (input_line ich)) :: data
      done
    with
	End_of_file -> List.rev data
