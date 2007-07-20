
type 'a hbuf

val create : 'a array -> 'a hbuf

val subbuf : 'a hbuf -> int -> int -> 'a hbuf

val concat : 'a hbuf -> 'a hbuf -> 'a hbuf

val nth : 'a hbuf -> int -> 'a


  
