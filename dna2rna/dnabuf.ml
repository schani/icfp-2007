type 'a hbuf =
    ArrayBuf of 'a array
  | SubBuf of 'a hbuf * int * int
  | ConcatBuf of 'a hbuf * 'a hbuf * int;;

(* val create : 'a array -> 'a hbuf *)
let create arr =
  ArrayBuf arr;;

(* val length : 'a hbuf -> int *)
let rec length = function
    ArrayBuf arr -> Array.length arr
  | SubBuf (_, start, stop) -> stop - start
  | ConcatBuf (_, _, len) -> len;;

(* val subbuf : 'a hbuf -> int -> int -> 'a hbuf *)
let subbuf hbuf start stop =
  (assert (stop >= start);
   assert (start >= 0);
   assert (stop <= (length hbuf));
   SubBuf (hbuf, start, stop));;

(* val concat : 'a hbuf -> 'a hbuf -> 'a hbuf *)
let concat buf1 buf2 =
  let len1 = length buf1
  and len2 = length buf2
  in ConcatBuf (buf1, buf2, (len1 + len2));;

(* val nth : 'a hbuf -> int -> 'a *)
let rec nth hbuf i =
  (assert (i >= 0);
   assert (i < (length hbuf));
   match hbuf with
       ArrayBuf arr -> arr.(i)
     | SubBuf (hbuf, start, _) -> nth hbuf (i - start)
     | ConcatBuf (buf1, buf2, _) ->
	 let len1 = length buf1
	 in if i < len1 then
	     nth buf1 i
	   else
	     nth buf2 (i - len1));;

(* val consume : 'a hbuf -> ('a * ('a hbuf)) option *)
let consume hbuf =
  let rec rest hbuf =
    let len = length hbuf
    in match hbuf with
	ArrayBuf arr -> SubBuf (hbuf, 1, len)
      | SubBuf (sub, start, stop) -> SubBuf (sub, start + 1, stop)
      | ConcatBuf (buf1, buf2, len) ->
	  if (length buf1) < 1 then
	    rest buf2
	  else
	    ConcatBuf (rest buf1, buf2, len - 1)
  in    
    if (length hbuf) > 0 then
      Some (nth hbuf 0, rest hbuf)
    else
      None;;
