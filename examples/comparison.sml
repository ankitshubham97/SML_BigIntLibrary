use "../biginteger.sml";

val a ="15" ;
val b ="5" ;
val a_big = bigstruct.str2bi(a);
val b_big = bigstruct.str2bi(b);

val ab_eq = bigstruct.eq(a_big,b_big);
(* ab_eq should evaluate to false*)

val ab_neq = bigstruct.neq(a_big,b_big);
(* ab_neq should evaluate to true*)

val ab_gt = bigstruct.gt(a_big,b_big);
(* ab_gt should evaluate to true*)

val ab_geq = bigstruct.geq(a_big,b_big);
(* ab_geq should evaluate to true*)

val ab_lt = bigstruct.lt(a_big,b_big);
(* ab_lt should evaluate to false*)

val ab_leq = bigstruct.leq(a_big,b_big);
(* ab_leq should evaluate to false*)