use "../biginteger.sml";

val a ="15" ;
val b ="5" ;
val a_big = bigstruct.str2bi(a);
val b_big = bigstruct.str2bi(b);
val c_big = bigstruct.mod4bigint(a_big,b_big);
val c = bigstruct.bi2str(c_big);
(*c should evaluate to 0*)