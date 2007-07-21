exception Invalid_base_char;;
exception Buffer_too_long;;

type base = I | C | F | P;;

type dna =
    ArrayBuf of Buffer.t * int * bool
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

(* val create : string -> bool -> dna *)
let create str from_file =
  let len = String.length str
  in let buf = Buffer.create len
  in (Buffer.add_string buf str;
      ArrayBuf (buf, len, from_file));;

let empty_dna = create "" false;;

(* val dna_from_base : base -> dna *)
let dna_from_base b =
  let buf = Buffer.create 16
  in (Buffer.add_char buf (base_to_char b);
      ArrayBuf (buf, 1, false));;

(* val length : dna -> int *)
let rec length = function
    ArrayBuf (_, len, _) -> len
  | SubBuf (_, start, stop) -> stop - start
  | ConcatBuf (_, _, len) -> len;;

(* val subbuf : dna -> int -> int -> dna *)
let rec subbuf hbuf start stop =
  if start = stop then
    empty_dna
  else
    (assert (stop > start);
     assert (start >= 0);
     assert (stop <= (length hbuf));
     match hbuf with
	 ArrayBuf _ -> SubBuf (hbuf, start, stop)
       | SubBuf (sub, sub_start, sub_stop) ->
	   subbuf sub (sub_start + start) (sub_start + stop)
       | ConcatBuf (buf1, buf2, _) ->
	   let len1 = length buf1
	   in if start >= len1 then
	       subbuf buf2 (start - len1) (stop - len1)
	     else if stop <= len1 then
	       subbuf buf1 start stop
	     else
	       SubBuf (hbuf, start, stop));;

     (*ConcatBuf ((subbuf buf1 start len1), (subbuf buf2 0 (stop - len1)), stop - start));; *)

let num_concat_same = ref 0;;

(* val concat : dna -> dna -> dna *)
let concat buf1 buf2 =
  let len1 = length buf1
  and len2 = length buf2
  in (match buf1 with
	  ConcatBuf (_, buf12, _) -> if buf12 == buf2 then num_concat_same := !num_concat_same + 1
	| _ -> ());
    ConcatBuf (buf1, buf2, (len1 + len2));;

(* val concat_base : dna -> base -> dna *)
let rec concat_base buf b =
  match buf with
      ArrayBuf (arr, len, from_file) ->
	if (not from_file) && ((Buffer.length arr) = len) then
	  (Buffer.add_char arr (base_to_char b);
	   ArrayBuf (arr, len + 1, false))
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
       ArrayBuf (arr, _, _) -> char_to_base (Buffer.nth arr i)
     | SubBuf (hbuf, start, _) -> nth hbuf (start + i)
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
      empty_dna
    else
      match hbuf with
	  ArrayBuf (arr, len, _) -> SubBuf (hbuf, i, len)
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

let rec flatten buf =
  let buf_len = length buf
  and num_orig = ref 0
  in if buf_len > Sys.max_string_length / 2 then
      let half = buf_len / 2
      in concat (flatten (subbuf buf 0 half)) (flatten (subbuf buf half buf_len))
    else
      let new_buf = Buffer.create buf_len
      in let rec blit buf start stop =
	  match buf with
	      ArrayBuf (buf, len, from_file) ->
		(assert (stop >= start);
		 assert (start >= 0);
		 assert (stop <= len);
		 assert ((Buffer.length new_buf) + (stop - start) <= buf_len);
		 if from_file then
		   num_orig := !num_orig + 1;
		 if stop > start then
		   Buffer.add_string new_buf (Buffer.sub buf start (stop - start)))
	    | SubBuf (sub, sub_start, _) ->
		blit sub (sub_start + start) (sub_start + stop)
	    | ConcatBuf (buf1, buf2, len) ->
		let len1 = length buf1
		in (if start < len1 then
		      blit buf1 start (min len1 stop);
		    if stop > len1 then
		      blit buf2 (max 0 (start - len1)) (stop - len1))
      in
	(* print_string "flattening "; print_int buf_len; print_newline (); *)
	blit buf 0 buf_len;
	print_string "done flattening: "; print_int !num_orig; print_string " same: "; print_int !num_concat_same;  print_newline ();
	ArrayBuf (new_buf, buf_len, false);;

let rec dna_to_string buf =
  let buf = flatten buf
  in match buf with
      ArrayBuf (buf, _, _) ->
	Buffer.contents buf
    | _ -> raise Buffer_too_long;;

let rec buffer_index_from buf start char =
  if start >= Buffer.length buf then
    None
  else if (Buffer.nth buf start) == char then
    Some start
  else
    buffer_index_from buf (start + 1) char;;

(* val search : dna -> dna -> int option *)
let search pat dna =
  let first = base_to_char (nth pat 0)
  and pat_len = length pat
  and pat_string = dna_to_string pat
  in let rec buffer_matches buf start index stop =
      if (start >= pat_len) || (index >= stop) then
	 true
       else if (Buffer.nth buf index) == (String.get pat_string start) then
	 buffer_matches buf (start + 1) (index + 1) stop
       else
	 false
     and matches dna start index stop =
      match dna with
	  ArrayBuf (buf, _, _) -> buffer_matches buf start index stop
	| SubBuf (sub, sub_start, sub_stop) -> matches sub start (sub_start + index) (sub_start + stop)
	| ConcatBuf (buf1, buf2, _) ->
	    let len1 = length buf1
	    in if stop <= len1 then
		matches buf1 start index stop
	      else if index < len1 then
		(matches buf1 start index len1) && (matches buf2 (start + len1 - index) 0 (stop - len1))
	      else
		matches buf2 start (index - len1) (stop - len1)
     and search dna start stop =
      if start >= stop then
	None
      else
	match dna with
	    ArrayBuf (arr, _, _) ->
	      (match buffer_index_from arr start first with
		   Some index ->
		     if (index < stop) && (matches dna 0 index stop) then
		       Some index
		     else
		       search dna (index + 1) stop
		 | None -> None)
	  | SubBuf (sub, sub_start, sub_stop) ->
	      (match search sub (sub_start + start) (sub_start + stop) with
		   Some index -> Some (index - sub_start)
		 | None -> None)
	  | ConcatBuf (buf1, buf2, len) ->
	      let len1 = length buf1
	      in if start < len1 then
		  match search buf1 start (min len1 stop) with
		      Some index ->
			if matches dna 0 index stop then
			  Some index
			else
			  search dna (index + 1) stop
		    | None ->
			(match search buf2 0 (stop - len1) with
			     Some index -> Some (index + len1)
			   | None -> None)
		else
		  (match search buf2 (start - len1) (stop - len1) with
		       Some index -> Some (index + len1)
		     | None -> None)
  in let len = (length dna)
  in match search dna 0 (length dna) with
      Some index ->
	if index + pat_len > len then
	  None
	else
	  Some index
    | None -> None;;

assert ((search (create "IFI" false) (create "PIPFIFIFI" false)) = (Some 4));;
assert ((search (create "IFI" false) (create "PIPFIFI" false)) = (Some 4));;
assert ((search (create "IFI" false) (concat (create "PIPFI" false) (create "FIF" false))) = (Some 4));;
assert ((search (create "IFI" false) (concat (create "PIPI" false) (create "PIFIF" false))) = (Some 5));;
assert ((search (create "IFI" false) (concat (subbuf (create "PIPICP" false) 1 4) (subbuf (create "PCFIPIFIP" false) 2 6))) = (Some 2));;
assert ((search (create "I" false)   (concat (create "P" false) (create "P" false))) = None);;
(* assert ((search (create "I" false)   (concat (create "P" false) (create "P" false)))= (Some 0));; *)
(* assert ((search (create "I" false)   (concat (subbuf (create "P" false) 0 1) (create "P" false)))= (Some 0));; *)

(* val read_dna : in_channel -> dna *)
let read_dna file =
  create (input_line file) true;;

(* val write_dna : dna -> out_channel -> unit *)
let write_dna buf file =
  let rec write buf start stop =
    match buf with
	ArrayBuf (buf, _, _) ->
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

let rec visualize = function
    ArrayBuf (_, len, _) -> print_int len
  | SubBuf (sub, start, stop) -> print_string "("; visualize sub; print_string ","; print_int start; print_string ":"; print_int stop; print_string ")"
  | ConcatBuf (buf1, buf2, len) -> visualize buf1; print_string "+"; visualize buf2;;

let history_info buf =
  let rec history buf start stop =
    match buf with
	ArrayBuf (_, _, false) -> []
      | ArrayBuf (_, _, true) -> [(start, stop)]
      | SubBuf (sub, sub_start, sub_stop) ->
	  history sub (sub_start + start) (sub_start + stop)
      | ConcatBuf (buf1, buf2, len) ->
	  let len1 = length buf1
	  in (if start < len1 then
		history buf1 start (min len1 stop)
	      else
		[]) @
	       (if stop > len1 then
		  history buf2 (max 0 (start - len1)) (stop - len1)
		else
		  [])
  in
    history buf 0 (length buf);;
