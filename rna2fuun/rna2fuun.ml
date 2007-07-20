(* Module Rna2fuun
 *
 * Ocaml Rna to Fuun converter
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

type rna_instr =
    | RI_Color color
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

let black : rgb = (0, 0, 0)
let red : rgb = (255, 0, 0)
let green : rgb = (0, 255, 0)
let yellow : rgb = (255, 255, 0)
let blue : rgb = (0, 0, 255)
let magenta : rgb = (255, 0, 255)
let cyan : rgb = (0, 255, 255)
let white : rgb = (255, 255, 255)

let transparent : transparency = 0
let opaque : transparency = 255

let createTransparentBitmap () =
  let transparentPixel = ((black, transparent) : pixel)
  in
    ((Array.create 600 (Array.create 600 transparentPixel)) : bitmap)

let bucket : bucket ref = ref []
let position : pos ref = ref (0, 0)
let mark : pos ref = ref (0, 0)
let dir = ref E
let bitmaps : bitmap list ref = ref []

let addColor c =
  bucket := c :: !bucket

let currentPixel () =
  let average filter_fun default = function
      [] -> default
    | l -> (List.fold_left (+) 0 (List.map filter_fun l)) / (List.length l)
  let rc = average (fun RGB (r,g,b) -> r | Alpha _ -> 0) 0
  let gc = average (fun RGB (r,g,b) -> g | Alpha _ -> 0) 0
  let bc = average (fun RGB (r,g,b) -> b | Alpha _ -> 0) 0
  let ac = average (fun RGB _ -> 0 | Alpha a -> a) 255
  in
    (rc * ac / 255, gc * ac / 255, bc * ac / 255)

let move ((x,y) : pos) = function
    N -> ((x, (y - 1) mod 600) : pos)
  | E -> (x + 1) mod 600, y
  | S -> x, (y + 1) mod 600
  | W -> (x - 1) mod 600, y

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

let getPixel ((x,y) : pos) =
  let bm0 = List.hd !bitmaps
  in
    bm0.(y).(x)

let setPixel ((x,y) : pos) =
  let bm0 = List.hd !bitmaps
  in
    bm0.(y).(x) <- currentPixel ()

let line ((x0, y0) : pos) ((x1, y1) : pos) =
  let deltax = x1 - x0
  and deltay = x1 - y0
  in let d = max (abs deltax) (abs deltay)
  and c = if (deltax * deltay <= 0) then 1 else 0
  in let x = ref (x0 * d + floor ((float_of_int (d - c)) /. 2.0))
  in let y = ref (y0 * d + floor ((float_of_int (d - c)) /. 2.0))
  in
    for i = 1 to d do
      setPixel (floor ((float_of_int x) /. (float_of_int d)),
	       floor  ((float_of_int y) /. (float_of_int d)));
      x := x + deltax;
      y := y + deltay;
    done;
    setPixel (x1, y1)

let tryfill () =
  let new = currentPixel ()
  and old = getPixel position
  in
    if new != old
    then
      fill position old

let fill (p : pos) (initial : pixel) =
  let x,y = p
  in
    if getPixel p == initial then
      begin
	if x > 0 then fill (x - 1, y) initial;
	if x < 599 then fill (x + 1, y) initial;
	if y > 0 then fill (x, y - 1) initial;
	if y < 599 then fill (x, y + 1) initial;
      end

let addBitmap (b : bitmap) =
  if (List.length !bitmaps) < 10 then
    bitmaps := b :: !bitmaps

let compose () =
  if (List.length !bitmaps) >= 2 then
    let bm0 = List.hd !bitmaps
    and bm1 = List.hd (List.tl !bitmaps)
    and merge y x p0 =
      let ((r0, g0, b0), a0) = p0
      and ((r1, g1, b1), a1) = bm1.(y).(x)
      in
	bm1.(y).(x) <-
	  (((r0 + floor((float_of_int r1) *.
			   (float_of_int (255 - a0) /. 255.0))),
	   (g0 + floor((float_of_int g1) *.
			  (float_of_int (255 - a0) /. 255.0))),
	   (b0 + floor((float_of_int b1) *.
			  (float_of_int (255 - a0) /. 255.0)))),
	  (a0 + floor((float_of_int a1) *.
			 (float_of_int (255 - a0) /. 255.0)))
    in
      Array.iteri (fun y bm0l -> Array.iteri (merge y) bm0l) bm0;
      bitmaps := List.tl !bitmaps

let clip () =
  if (List.length !bitmaps) => 2 then
    let merge y x p0 =
      let ((r0, g0, b0), a0) = p0
      and ((r1, g1, b1), a1) = bm1.(y).(x)
      in
	bm1.(y).(x) <-
	  ((floor((float_of_int r1) *. (float_of_int a0) /. 255.0),
	   floor((float_of_int g1) *. (float_of_int a0) /. 255.0),
	   floor((float_of_int b1) *. (float_of_int a0) /. 255.0)),
	  floor((float_of_int a1) *. (float_of_int a0) /. 255.0))
    in
      Array.iteri (fun y bm0l -> Array.iteri (merge y) bm0l) bm0;

let appy_instr = function
  | RI_Color c -> addColor c
  | RI_ClearBucket -> bucket := []
  | RI_Move -> position :=
  | RI_RotateCounterClockwise -> dir := turnCounterClockwise dir
  | RI_RotateClockwise -> dir := turnClockwise dir
  | RI_Mark -> mark := position
  | RI_Line -> line position mark
  | RI_Fill -> tryfill ()
  | RI_AddBitmap -> addBitmap (createTransparentBitmap ())
  | RI_Compose -> compose ()
  | RI_Clip -> clip ()

let usage () =
  fprintf stderr "usage not yet written, sorry";
  exit 1
	  
let _ =
  if Array.length Sys.argv != 2 then
    usage ();
  Parseintern.read_trace Sys.argv.(1)
