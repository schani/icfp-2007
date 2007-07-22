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
    | '?' -> RI_Ignore (Str.string_after str 1)
    | _ -> assert false

let parse_fast ich =
  let newline_regexp = Str.regexp "[\n]+"
  in let file_len = in_channel_length ich
  in let buf = String.create file_len
  in
    really_input ich buf 0 file_len;
    let strings = Str.split newline_regexp buf
    in
      List.map parse_line strings

let read_trace filename =
  let ich = open_in filename
  in
    if in_channel_length ich < 1024*1024*15
    then
      parse_fast ich
    else
      let data = ref []
      in
	try
	  while true do
	    data := (parse_line (input_line ich)) :: !data
	  done;
	  !data
	with
	    End_of_file -> List.rev !data
