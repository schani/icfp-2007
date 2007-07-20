exception Invalid_base_char;;

type base = I | C | F | P;;

type dna =
    ArrayBuf of Buffer.t * int
  | SubBuf of dna * int * int
  | ConcatBuf of dna * dna * int;;

let char_to_base = function
    'I' -> I
  | 'C' -> C
  | 'F' -> F
  | 'P' -> P
  | _ -> raise Invalid_base_char;;

let base_to_char = function
    I -> 'I'
  | C -> 'C'
  | F -> 'F'
  | P -> 'P';;

(* val create : string -> dna *)
let create str =
  let len = String.length str
  in let buf = Buffer.create len
  in (Buffer.add_string buf str;
      ArrayBuf (buf, len));;

(* val dna_from_base : base -> dna *)
let dna_from_base b =
  let buf = Buffer.create 16
  in (Buffer.add_char buf (base_to_char b);
      ArrayBuf (buf, 1));;

(* val length : dna -> int *)
let rec length = function
    ArrayBuf (_, len) -> len
  | SubBuf (_, start, stop) -> stop - start
  | ConcatBuf (_, _, len) -> len;;

(* val subbuf : dna -> int -> int -> dna *)
let subbuf hbuf start stop =
  (assert (stop >= start);
   assert (start >= 0);
   assert (stop <= (length hbuf));
   SubBuf (hbuf, start, stop));;

let empty_dna =
  subbuf (dna_from_base I) 1 1;;

(* val concat : dna -> dna -> dna *)
let concat buf1 buf2 =
  let len1 = length buf1
  and len2 = length buf2
  in ConcatBuf (buf1, buf2, (len1 + len2));;

(* val concat_base : dna -> base -> dna *)
let rec concat_base buf b =
  match buf with
      ArrayBuf (arr, len) ->
	if (Buffer.length arr) = len then
	  (Buffer.add_char arr (base_to_char b);
	   ArrayBuf (arr, len + 1))
	else
	  ConcatBuf (buf, dna_from_base b, len + 1)
    | SubBuf (sub, start, stop) ->
	ConcatBuf (buf, dna_from_base b, stop - start + 1)
    | ConcatBuf (buf1, buf2, len) ->
	ConcatBuf (buf1, concat_base buf2 b, len + 1);;

(* val nth : dna -> int -> base *)
let rec nth hbuf i =
  (assert (i >= 0);
   assert (i < (length hbuf));
   match hbuf with
       ArrayBuf (arr, len) -> char_to_base (Buffer.nth arr i)
     | SubBuf (hbuf, start, _) -> nth hbuf (i - start)
     | ConcatBuf (buf1, buf2, _) ->
	 let len1 = length buf1
	 in if i < len1 then
	     nth buf1 i
	   else
	     nth buf2 (i - len1));;

(* val skip : dna -> int -> dna *)
let rec skip hbuf i =
  let len = length hbuf
  in if len <= i then
      SubBuf (hbuf, len, len)
    else
      match hbuf with
	  ArrayBuf (arr, len) -> SubBuf (hbuf, i, len)
	| SubBuf (sub, start, stop) -> SubBuf (sub, start + i, stop)
	| ConcatBuf (buf1, buf2, len) ->
	    let len1 = length buf1
	    in if (length buf1) < i then
		skip buf2 (i - len1)
	      else
		ConcatBuf (skip buf1 i, buf2, len - i);;

(* val consume : dna -> (base * dna) option *)
let consume hbuf =
  if (length hbuf) > 0 then
    Some (nth hbuf 0, skip hbuf 1)
  else
    None;;

(* val read_dna : in_channel -> dna *)
let read_dna file =
  create (input_line file);;

(* val write_dna : dna -> out_channel -> unit *)
let write_dna buf file =
  let rec write buf start stop =
    match buf with
	ArrayBuf (buf, len) ->
	  output_string file (Buffer.sub buf start (stop - start))
      | SubBuf (sub, sub_start, _) ->
	  write sub (sub_start + start) (sub_start + stop)
      | ConcatBuf (buf1, buf2, len) ->
	  let len1 = length buf1
	  in (if start < len1 then
		write buf1 start (min len1 stop);
	      if stop > len1 then
		write buf2 (max 0 (start - len1)) (stop - len1))
  in
    write buf 0 (length buf);;
