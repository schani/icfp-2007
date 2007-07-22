
open Rna
open Printf

(*
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
type fastbucket = {
  mutable fb_pixel : pixel;
  mutable fb_rgb_count : int;
  mutable fb_transparency_count : int;
  mutable fb_getPixel : ((int * int * int) * int) lazy_t;
}

type dir = N | E | S | W

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

let createEmptyFastBucket () =
  {
    fb_pixel = (0, 0, 0), 0;
    fb_rgb_count = 0;
    fb_transparency_count = 0;
    fb_getPixel = lazy ((0,0,0),255)
  }

let pixel_from_fastbucket fb =
  Lazy.force fb.fb_getPixel

let really_pixel_from_fastbucket fb =
  fprintf stdout "computing it really\n";
  flush stdout;
  let (r,g,b),a = fb.fb_pixel
  in let rc = if fb.fb_rgb_count > 0 then r / fb.fb_rgb_count else 0
  in let gc = if fb.fb_rgb_count > 0 then g / fb.fb_rgb_count else 0
  in let bc = if fb.fb_rgb_count > 0 then b / fb.fb_rgb_count else 0
  in let ac = if fb.fb_transparency_count > 0 then
    a / fb.fb_transparency_count else 255
  in
      (rc * ac / 255, gc * ac / 255, bc * ac / 255), ac

let colorAdd fb (rd,gd,bd) =
  let (r,g,b),a = fb.fb_pixel
  in let result = { fb with
    fb_pixel = ((r+rd, g+gd, b+bd), a);
    fb_rgb_count = fb.fb_rgb_count + 1
  }
  in
    result.fb_getPixel <- lazy (really_pixel_from_fastbucket result);
    result

let alphaAdd fb ad =
  let (r,g,b),a = fb.fb_pixel
  in let result = { fb with
    fb_pixel = ((r, g, b), a+ad);
    fb_transparency_count= fb.fb_transparency_count + 1
  }
  in
    result.fb_getPixel <- lazy (really_pixel_from_fastbucket result);
    result

let dumpPixel p =
  let (r,g,b),a = p
  in
    fprintf stdout "Bucket(R=%i, G=%i, B=%i, A=%i)\n" r g b a;
    flush stdout
*)
let b = ref (createEmptyFastBucket ())

let _ =
  dumpPixel (pixel_from_fastbucket !b);
  b := alphaAdd !b transparent;
  b := alphaAdd !b opaque;
  b := alphaAdd !b opaque;
  dumpPixel (pixel_from_fastbucket !b);
  b := createEmptyFastBucket ();
  b := colorAdd !b black;
  b := colorAdd !b yellow;
  b := colorAdd !b cyan;
  dumpPixel (pixel_from_fastbucket !b);
  b := createEmptyFastBucket ();
  b := colorAdd !b yellow;
  b := alphaAdd !b transparent;
  b := alphaAdd !b opaque;
  dumpPixel (pixel_from_fastbucket !b);
  b := createEmptyFastBucket ();
  for i = 1 to 18 do b:= colorAdd !b black done;
  for i = 1 to 07 do b:= colorAdd !b red done;
  for i = 1 to 39 do b:= colorAdd !b magenta done;
  for i = 1 to 10 do b:= colorAdd !b white done;
  for i = 1 to 03 do b:= alphaAdd !b opaque done;
  for i = 1 to 01 do b:= alphaAdd !b transparent done;
  dumpPixel (pixel_from_fastbucket !b);
  b := createEmptyFastBucket ();
  for i = 1 to 9 do b:= colorAdd !b white done;
  dumpPixel (pixel_from_fastbucket !b);
  dumpPixel (pixel_from_fastbucket !b)
