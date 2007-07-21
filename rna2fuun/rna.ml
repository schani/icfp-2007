(* Module Rna
 *
 * RNA types and funs from the assignment
 *)

open Printf

type coord = int
type pos = coord * coord
type component = int
type rgb = component * component * component
type transparency = component
type pixel = rgb * transparency
type bitmap = pixel array array
type color = RGB of rgb
	     | Alpha of transparency
type bucket = color list
type dir = N | E | S | W

let string_of_dir = function
    N -> "N"
  | E -> "E"
  | S -> "S"
  | W -> "W"

type rna_instr =
    | RI_Color of color
    | RI_ClearBucket
    | RI_Move
    | RI_RotateCounterClockwise
    | RI_RotateClockwise
    | RI_Mark
    | RI_Line
    | RI_Fill
    | RI_AddBitmap
    | RI_Compose
    | RI_Clip
    | RI_Ignore

let black : rgb = (0, 0, 0)
let red : rgb = (255, 0, 0)
let green : rgb = (0, 255, 0)
let yellow : rgb = (255, 255, 0)
let blue : rgb = (0, 0, 255)
let magenta : rgb = (255, 0, 255)
let cyan : rgb = (0, 255, 255)
let white : rgb = (255, 255, 255)

let string_of_rgb = function
  | (0, 0, 0) -> "black"
  | (255, 0, 0) -> "red"
  | (0, 255, 0) -> "green"
  | (255, 255, 0) -> "yellow"
  | (0, 0, 255) -> "blue"
  | (255, 0, 255) -> "magenta"
  | (0, 255, 255) -> "cyan"
  | (255, 255, 255) -> "white"
  | r,g,b -> sprintf "RGB:(%i,%i,%i)" r g b

let string_of_color = function
  | RGB rgb -> string_of_rgb rgb
  | Alpha a -> sprintf "Alpha:%i" a

let string_of_instr = function
  | RI_Color c -> string_of_color c
  | RI_ClearBucket -> "ClearBucket"
  | RI_RotateClockwise -> "RotClockWise"
  | RI_RotateCounterClockwise -> "RotCounterCW"
  | RI_Move -> "Move"
  | RI_Mark -> "Mark"
  | RI_Line -> "Line"
  | RI_Fill -> "Fill"
  | RI_AddBitmap -> "AddBitmap"
  | RI_Compose -> "Compose"
  | RI_Clip -> "Clip"
  | RI_Ignore -> "Ignore"

let transparent : transparency = 0
let opaque : transparency = 255

let createDemoBitmap () =
  let opaquePixel = ((black, opaque) : pixel)
  in let bm = ((Array.create 600 (Array.create 0 opaquePixel)) : bitmap)
  in
    for y = 0 to 599 do
      bm.(y) <- Array.create 600 opaquePixel;
      for x = 0 to 599 do
	bm.(y).(x) <- ((y, y, y), opaque)
      done
    done;
    bm

let createTransparentBitmap () =
  let transparentPixel = ((black, transparent) : pixel)
  in
    ((Array.create 600 (Array.create 600 transparentPixel)) : bitmap)

type rna_state = {
  mutable bucket : bucket;
  mutable position : pos;
  mutable mark : pos;
  mutable dir : dir;
  mutable bitmaps : bitmap list;
}

let createRNAState () =
  { bucket = []; position = 0, 0; mark = 0,0; dir = E; bitmaps = [createTransparentBitmap ()] }

let addColor st c =
  st.bucket <- c :: st.bucket

let currentPixel st =
  let average filter_fun default = function
      [] -> default
    | l -> (List.fold_left (+) 0 (List.map filter_fun l)) / (List.length l)
  in let rc = average (function RGB (r,g,b) -> r | Alpha _ -> 0) 0 st.bucket
  and gc = average (function RGB (r,g,b) -> g | Alpha _ -> 0) 0 st.bucket
  and bc = average (function RGB (r,g,b) -> b | Alpha _ -> 0) 0 st.bucket
  and ac = average (function RGB _ -> 0 | Alpha a -> a) 255 st.bucket
  in
       ((int_of_float (floor((float_of_int rc) *. (float_of_int ac) /. 255.0)),
	int_of_float (floor((float_of_int gc) *. (float_of_int ac) /. 255.0)),
	int_of_float (floor((float_of_int bc) *. (float_of_int ac) /. 255.0))),
	ac : pixel)

let move ((x,y) : pos) = function
    N -> ((x, (y - 1 + 600) mod 600) : pos)
  | E -> (x + 1) mod 600, y
  | S -> x, (y + 1) mod 600
  | W -> (x - 1 + 600) mod 600, y

let turnCounterClockwise = function
  N -> W
| E -> N
| S -> E
| W -> S

let turnClockwise = function
  N -> E
| E -> S
| S -> W
| W -> N

let getPixel st ((x,y) : pos) =
  let bm0 = List.hd st.bitmaps
  in
    bm0.(y).(x)

let setPixel st ((x,y) : pos) =
  let bm0 = List.hd st.bitmaps
  in
    bm0.(y).(x) <- currentPixel st

let line st ((x0, y0) : pos) ((x1, y1) : pos) =
  let deltax = x1 - x0
  and deltay = x1 - y0
  in let d = max (abs deltax) (abs deltay)
  and c = if (deltax * deltay <= 0) then 1 else 0
  in let x = ref (x0 * d + (int_of_float
			       (floor ((float_of_int (d - c)) /. 2.0))))
  and y = ref (y0 * d + (int_of_float
			    (floor ((float_of_int (d - c)) /. 2.0))))
  in
    for i = 1 to d do
      setPixel st
	((int_of_float (floor ((float_of_int !x) /. (float_of_int d)))),
	(int_of_float (floor ((float_of_int !y) /. (float_of_int d)))));
      x := !x + deltax;
      y := !y + deltay;
    done;
    setPixel st (x1, y1)

let rec fill st (initial : pixel) = function
    [] -> ()
  | (p : pos) :: ps ->
      let x, y = p
      in
	if getPixel st p == initial then
	  fill st initial
	    ((x - 1, y) :: (x + 1, y) :: (x, y - 1) :: (x, y + 1) :: ps)
	else
	  fill st initial ps
(*
let rec orig_fill st (p : pos) (initial : pixel) =
  let x,y = p
  in
    if getPixel st p == initial then
      begin
	if x > 0 then orig_fill st (x - 1, y) initial;
	if x < 599 then orig_fill st (x + 1, y) initial;
	if y > 0 then orig_fill st (x, y - 1) initial;
	if y < 599 then orig_fill st (x, y + 1) initial;
      end
*)
let tryfill st =
  let neu = currentPixel st
  and old = getPixel st st.position
  in
    if neu != old
    then
      fill st old [st.position]

let addBitmap st (b : bitmap) =
  if (List.length st.bitmaps) < 10 then
    st.bitmaps <- b :: st.bitmaps

let compose st =
  if (List.length st.bitmaps) >= 2 then
    let bm0 = List.hd st.bitmaps
    and bm1 = List.hd (List.tl st.bitmaps)
    in let merge y x p0 =
      let ((r0, g0, b0), a0) = p0
      and ((r1, g1, b1), a1) = bm1.(y).(x)
      in
	bm1.(y).(x) <-
	  (((r0 + (int_of_float (floor ((float_of_int r1) *.
					   (float_of_int (255 - a0)
					     /. 255.0))))),
	   (g0 + (int_of_float (floor ((float_of_int g1) *.
					  (float_of_int (255 - a0)
					    /. 255.0))))),
	   (b0 + (int_of_float (floor ((float_of_int b1) *.
					  (float_of_int (255 - a0)
					    /. 255.0)))))),
	  (a0 + (int_of_float (floor ((float_of_int a1) *.
					 (float_of_int (255 - a0)
					   /. 255.0))))))
    in
      Array.iteri (fun y bm0l -> Array.iteri (merge y) bm0l) bm0;
      st.bitmaps <- List.tl st.bitmaps

let clip st =
  if (List.length st.bitmaps) >= 2 then
    let bm0 = List.hd st.bitmaps
    and bm1 = List.hd (List.tl st.bitmaps)
    in let merge y x p0 =
      let ((r0, g0, b0), a0) = p0
      and ((r1, g1, b1), a1) = bm1.(y).(x)
      in
	bm1.(y).(x) <-
	  (((int_of_float (floor ((float_of_int r1) *. (float_of_int a0)
				   /. 255.0))),
	   (int_of_float (floor ((float_of_int g1) *. (float_of_int a0)
				  /. 255.0))),
	   (int_of_float (floor ((float_of_int b1) *. (float_of_int a0)
				  /. 255.0)))),
	  (int_of_float (floor ((float_of_int a1) *. (float_of_int a0)
				 /. 255.0))))
    in
      Array.iteri (fun y bm0l -> Array.iteri (merge y) bm0l) bm0;
      st.bitmaps <- List.tl st.bitmaps

let apply_instr st instr =
  try
    match instr with
      | RI_Color c -> addColor st c
      | RI_ClearBucket -> st.bucket <- []
      | RI_Move -> st.position <- move st.position st.dir
      | RI_RotateCounterClockwise -> st.dir <- turnCounterClockwise st.dir
      | RI_RotateClockwise -> st.dir <- turnClockwise st.dir
      | RI_Mark -> st.mark <- st.position
      | RI_Line -> line st st.position st.mark
      | RI_Fill -> tryfill st
      | RI_AddBitmap -> addBitmap st (createTransparentBitmap ())
      | RI_Compose -> compose st
      | RI_Clip -> clip st
      | RI_Ignore -> ()
  with
      x -> fprintf stderr "execute error during %s: %s\n"
	(string_of_instr instr) (Printexc.to_string x);
	flush stderr


