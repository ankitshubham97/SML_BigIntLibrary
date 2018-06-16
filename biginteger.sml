
Control.Print.printLength := 500;
signature BigInt =
sig
type bigint=int list
val getbigint: int -> bigint
val bi2str : bigint -> string
val str2bi : string -> bigint
val lt : bigint * bigint -> bool
val leq : bigint * bigint -> bool
val gt : bigint * bigint -> bool
val geq : bigint * bigint -> bool
val eq : bigint * bigint -> bool
val neq : bigint * bigint -> bool
val add : bigint * bigint -> bigint
val sub : bigint * bigint -> bigint
val mul : bigint * bigint -> bigint
val div4bigint : bigint * bigint -> bigint
val mod4bigint : bigint * bigint -> bigint(**)
val unminus : bigint -> bigint
end;

structure bigstruct : BigInt = 
struct
exception Divbyzero
type bigint = int list
fun getbigint  i = 
			let
				fun convert int_t list_t = if (int_t =0) then list_t else convert (int_t div 10) ([int_t mod 10]@list_t);
				val final = if (i >= 0) then (convert i [])@[0] else (convert (~(i)) [])@[1];
			in (final)
			end ;
fun bi2str i1=
			let
				fun remove_leading_zeros x = if (hd(x)=0 andalso tl(x)<>[0]) then (remove_leading_zeros (tl(x))) else x;
				val i = remove_leading_zeros i1;
				fun convert bigint_t string_t = if (bigint_t =[]) then string_t else convert (rev (tl(rev bigint_t))) ([chr(hd(rev(bigint_t))+48)]@string_t);
				val kk = implode (convert i []);
				val k = 
					if(i=[0,1] orelse i=[0,0])
					then "0"
					else if (hd(rev(i)) = 0) 
					then String.substring(kk,0,(size kk) -1) 
					else "-"^String.substring(kk,0,(size kk) -1);
			in (k)
			end ;
fun str2bi i=
			let
				fun convert string_t bigint_t= if (size (string_t) =0) then bigint_t else convert (implode( rev (tl(rev (explode string_t))))) ([(ord(hd(rev(explode string_t)))-48)] @ bigint_t);
				val k = if(String.substring(i,0,1) = "-") then ((convert (String.extract(i,1,NONE)) [])@[1]) else (convert i [])@[0];
			in (k)
			end ;
fun lt (a1,b1) =
			let
				val a = str2bi(bi2str(a1));
				val b = str2bi(bi2str(b1));
				fun lteqlen a b = if(length a <> 0) 
					then (
						if ( hd((a)) < hd((b)) ) 
							then true
							else if ( hd((a)) > hd((b)) )
							then false 
							else lteqlen ((tl((a)))) ((tl((b)))) )
					else false;
				fun ltsize a b = if(length a < length b) then true else false;
				fun ltsign a b = if( hd(rev(a))=1 andalso hd(rev(b))=0 ) then true else false;
				val k = 
						if(a=[0,0] andalso b=[0,1])orelse(b=[0,0] andalso a=[0,1]) then false
						else if a=b then false
						else if ( hd(rev(a))=0 andalso hd(rev(b))=0 ) 
							then (if (length a = length b)
									then lteqlen a b
									else ltsize a b)
						else if ( hd(rev(a))=1 andalso hd(rev(b))=1 )
							then (if (length a = length b)
									then not(lteqlen (rev(tl(rev(a)))) (rev(tl(rev(b)))))
									else not(ltsize (rev(tl(rev(a)))) (rev(tl(rev(b))))))
						
						else  ltsign a b;
			in(k)
			end;
fun eq (a,b)= if(a=[0,0] andalso b=[0,1])orelse(b=[0,0] andalso a=[0,1]) then true
						else a=b; 
fun neq (a,b)=not(eq(a,b)); 
fun leq (a,b)=eq(a,b)orelse lt(a,b);
fun gt (a,b)=not(leq(a,b));
fun geq (a,b) = not(lt(a,b) );
fun unminus a = if(hd(rev(a))=1)
				then (
					rev(0::(tl(rev(a)))))
				else rev(1::(tl(rev(a))));
fun add (a , b)  = 
	let
		fun generate_continuous_list str num outp = 
			if num = 0 then outp else generate_continuous_list (str) (num-1) (str::outp);
		val size_a_minus_size_b = length a - length b;
		val a_size_adjusted = if(size_a_minus_size_b > 0) then a else generate_continuous_list (0) (~size_a_minus_size_b) a;
		val b_size_adjusted = if(size_a_minus_size_b < 0) then b else generate_continuous_list (0) (size_a_minus_size_b) b;
		fun is_a_greater_than_b a b  = 
			if (length a <>0)
			then(
				if ((hd(a)) > (hd(b))) then true 
				else if((hd(a)) < (hd(b))) then false
				else (is_a_greater_than_b (tl(a)) (tl(b)) )
				)
			else false;
		fun subst (a,b,c,d,e,res,cnt,fin)=
				if(cnt <> length a -2)
				then(
					if(d<e)
					then (subst(a,b,List.nth(rev (a),cnt+2),c-1,List.nth(rev(b),cnt+1),10+d-e,cnt+1, (Int.toString res)^fin ) )
					else (subst(a,b,List.nth(rev(a),cnt+2),c,List.nth(rev(b),cnt+1),d-e,cnt+1,(Int.toString res)^fin   ) )
				)
				else substring(((Int.toString res)^fin),0, size ((Int.toString res)^fin)-1);
		fun subeqlen1 x y res= 
			if (length x <>0)
			then subeqlen1 (rev(tl(rev(x) ) )) (rev(tl(rev(y) ) )) ([((hd(rev(x)))-(hd(rev(y))))]@res)
			else res;
		fun subeqlen2 x prev_digit_sign res = 
			if(prev_digit_sign = 0 andalso hd(rev(x))>=0 andalso length x >1) 
			then (print (Int.toString(hd(rev(x))));(subeqlen2 (rev(tl(rev(x)))) 0 (((hd(rev(x))))::res)))
			else if (prev_digit_sign = 0 andalso hd(rev(x))<0 andalso length x >1)
			then (print (Int.toString(hd(rev(x))+10));subeqlen2 (rev(tl(rev(x)))) 1 ((((hd(rev(x)))+10))::res))
			else if (prev_digit_sign = 1 andalso hd(rev(x))>=0 andalso length x >1)
			then (print (Int.toString(hd(rev(x))-1));subeqlen2 (rev(tl(rev(x)))) 0 ((((hd(rev(x)))-1))::res))
			else if (prev_digit_sign = 1 andalso hd(rev(x))<0 andalso length x >1)
			then (print (Int.toString(hd(rev(x))+9));subeqlen2 (rev(tl(rev(x)))) 1 ((((hd(rev(x)))+9))::res)) 
			else if(prev_digit_sign = 0 andalso hd(rev(x))>=0 ) 
			then ((((hd(rev(x))))::res))
			 else if (prev_digit_sign = 0 andalso hd(rev(x))<0 )
			then (((((hd(rev(x)))+10))::res))
			else if (prev_digit_sign = 1 andalso hd(rev(x))>=0 )
			then (((((hd(rev(x)))-1))::res))
			else if (prev_digit_sign = 1 andalso hd(rev(x))<0 )
			then (((((hd(rev(x)))+9))::res))(**)
			else [];
		fun addeqlen x y z res =if (length x <>0)
									then(
										if(((hd(rev(x)))+(hd(rev(y)))+z) >9)
										then addeqlen (rev(tl(rev(x) ) )) (rev(tl(rev(y) ) )) 1 ([((hd(rev(x)))+(hd(rev(y)))+z-10)]@res)
										else addeqlen (rev(tl(rev(x) ) )) (rev(tl(rev(y) ) )) 0 ([((hd(rev(x)))+(hd(rev(y)))+z)]@res)
									)
									else(
										if(((hd(a_size_adjusted))+(hd(b_size_adjusted))+z) >9)
										then [1]@res
										else res);
		val sign_a = hd(rev(a));
		val sign_b = hd(rev(b));
		val a1 = rev(tl(rev(a_size_adjusted)));
		val b1 = rev(tl(rev(b_size_adjusted)));	
		val oper_a = [0,0]@a_size_adjusted;
		val oper_b = [0,0]@b_size_adjusted;
		val a11 = rev(tl(rev(oper_a)));
		val b11 = rev(tl(rev(oper_b)));
		val is_a1_greater_than_b1 = is_a_greater_than_b a1 b1;
		val stg1 = 
		
		(*else if(sign_a =0 andalso sign_b =0 andalso is_a1_greater_than_b1 =true )
			then( str2bi(subst (a11,b11,hd(tl(rev a11)),hd((rev a11)),hd((rev b11)),0,0 ,"") ))
			else if(sign_a =0 andalso sign_b =0 andalso is_a1_greater_than_b1 =false )
			then(str2bi( "-" ^ subst (b11,a11,hd(tl(rev b11)),hd((rev b11)),hd((rev a11)),0,0 ,"")) )
			else if(sign_a =0 andalso sign_b =1 andalso is_a1_greater_than_b1 =true )
			then( (addeqlen a1 b1 0 [] )@[0])
			else if(sign_a =0 andalso sign_b =1 andalso is_a1_greater_than_b1 =false )
			then( (addeqlen a1 b1 0 [] )@[0])
			else if(sign_a =1 andalso sign_b =0 andalso is_a1_greater_than_b1 =true )
			then( (addeqlen a1 b1 0 [] )@[1])
			else if(sign_a =1 andalso sign_b =0 andalso is_a1_greater_than_b1 =false )
			then( (addeqlen a1 b1 0 [] )@[1])
			else if(sign_a =1 andalso sign_b =1 andalso is_a1_greater_than_b1 =true )
			then(str2bi("-"^ subst (a11,b11,hd(tl(rev a11)),hd((rev a11)),hd((rev b11)),0,0 ,"")) )
			else if(sign_a =1 andalso sign_b =1 andalso is_a1_greater_than_b1 =false )
			then(str2bi( subst (b11,a11,hd(tl(rev b11)),hd((rev b11)),hd((rev a11)),0,0 ,"")))*)
			if(sign_a =0 andalso sign_b =0 andalso is_a1_greater_than_b1 =true )
			then( (addeqlen a1 b1 0 [] )@[0] )
			else if(sign_a =0 andalso sign_b =0 andalso is_a1_greater_than_b1 =false )
			then( (addeqlen a1 b1 0 [] )@[0] )
			else if(sign_a =0 andalso sign_b =1 andalso is_a1_greater_than_b1 =true )
			then( ( str2bi(subst (a11,b11,hd(tl(rev a11)),hd((rev a11)),hd((rev b11)),0,0 ,"") )))
			else if(sign_a =0 andalso sign_b =1 andalso is_a1_greater_than_b1 =false )
			then(str2bi( "-" ^ subst (b11,a11,hd(tl(rev b11)),hd((rev b11)),hd((rev a11)),0,0 ,"")) )
			else if(sign_a =1 andalso sign_b =0 andalso is_a1_greater_than_b1 =true )
			then (str2bi("-"^ subst (a11,b11,hd(tl(rev a11)),hd((rev a11)),hd((rev b11)),0,0 ,"")) )
			else if(sign_a =1 andalso sign_b =0 andalso is_a1_greater_than_b1 =false )
			then (str2bi( subst (b11,a11,hd(tl(rev b11)),hd((rev b11)),hd((rev a11)),0,0 ,"")))
			else if(sign_a =1 andalso sign_b =1 andalso is_a1_greater_than_b1 =true )
			then( (addeqlen a1 b1 0 [] )@[1] )
			else if(sign_a =1 andalso sign_b =1 andalso is_a1_greater_than_b1 =false )
			then( (addeqlen a1 b1 0 [] )@[1] )(**)
			else []; 
		fun remove_leading_zeros x = if (hd(x)=0 andalso tl(x)<>[0]) then (remove_leading_zeros (tl(x))) else x;
		val stg2 = remove_leading_zeros stg1;
	in (stg2)
	end;
	
	
fun sub (a,b) =
	let	
		fun generate_continuous_list str num outp = 
			if num = 0 then outp else generate_continuous_list (str) (num-1) (str::outp);
		fun is_a_greater_than_b a b  = 
			if (length a <>0)
			then(
				if ((hd(a)) > (hd(b))) then true 
				else if((hd(a)) < (hd(b))) then false
				else (is_a_greater_than_b (tl(a)) (tl(b)) )
				)
			else false;
			fun subst (a,b,c,d,e,res,cnt,fin)=
				if(cnt <> length a -2)
				then(
					if(d<e)
					then (subst(a,b,List.nth(rev (a),cnt+2),c-1,List.nth(rev(b),cnt+1),10+d-e,cnt+1, (Int.toString res)^fin ) )
					else (subst(a,b,List.nth(rev(a),cnt+2),c,List.nth(rev(b),cnt+1),d-e,cnt+1,(Int.toString res)^fin   ) )
				)
				else substring(((Int.toString res)^fin),0, size ((Int.toString res)^fin)-1);
			(*subst (a9,b9,hd(tl(rev a9)),hd((rev a9)),hd((rev b9)),0,0 ,"");*)
		fun subeqlen1 x y res= 
			if (length x <>0)
			then subeqlen1 (rev(tl(rev(x) ) )) (rev(tl(rev(y) ) )) ([((hd(rev(x)))-(hd(rev(y))))]@res)
			else res;
		fun subeqlen2 x prev_digit_sign res = 
		
		
			if(prev_digit_sign = 0 andalso hd(rev(x))>=0 andalso length x >1) 
			then ((subeqlen2 (rev(tl(rev(x)))) 0 (((hd(rev(x))))::res)))
			else if (prev_digit_sign = 0 andalso hd(rev(x))<0 andalso length x >1)
			then (subeqlen2 (rev(tl(rev(x)))) 1 ((((hd(rev(x)))+10))::res))
			else if (prev_digit_sign = 1 andalso hd(rev(x))>=0 andalso length x >1)
			then (subeqlen2 (rev(tl(rev(x)))) 0 ((((hd(rev(x)))-1))::res))
			else if (prev_digit_sign = 1 andalso hd(rev(x))<0 andalso length x >1)
			then (subeqlen2 (rev(tl(rev(x)))) 1 ((((hd(rev(x)))+9))::res)) 
			else if(prev_digit_sign = 0 andalso hd(rev(x))>=0 ) 
			then ((((hd(rev(x))))::res))
			 else if (prev_digit_sign = 0 andalso hd(rev(x))<0 )
			then (((((hd(rev(x)))+10))::res))
			else if (prev_digit_sign = 1 andalso hd(rev(x))>=0 )
			then (((((hd(rev(x)))-1))::res))
			else if (prev_digit_sign = 1 andalso hd(rev(x))<0 )
			then (((((hd(rev(x)))+9))::res))(**)
			else [];
		fun addeqlen x y z res =if (length x <>0)
									then(
										if(((hd(rev(x)))+(hd(rev(y)))+z) >9)
										then addeqlen (rev(tl(rev(x) ) )) (rev(tl(rev(y) ) )) 1 ([((hd(rev(x)))+(hd(rev(y)))+z-10)]@res)
										else addeqlen (rev(tl(rev(x) ) )) (rev(tl(rev(y) ) )) 0 ([((hd(rev(x)))+(hd(rev(y)))+z)]@res)
									)
									else(
										if(((hd(a))+(hd(b))+z) >9)
										then [1]@res
										else res);
		val size_a_minus_size_b = length a - length b;
		val a_size_adjusted = if(size_a_minus_size_b > 0) then a else generate_continuous_list (0) (~size_a_minus_size_b) a;
		val b_size_adjusted = if(size_a_minus_size_b < 0) then b else generate_continuous_list (0) (size_a_minus_size_b) b;	
		
		val sign_a = hd(rev(a));
		val sign_b = hd(rev(b));
		val a1 = rev(tl(rev(a_size_adjusted)));
		val b1 = rev(tl(rev(b_size_adjusted)));
		val oper_a = [0,0]@a_size_adjusted;
		val oper_b = [0,0]@b_size_adjusted;
		val a11 = rev(tl(rev(oper_a)));
		val b11 = rev(tl(rev(oper_b)));
		val is_a1_greater_than_b1 = is_a_greater_than_b a1 b1;
		val stg1 = 
			if(a=b)
			then [0,0]
			
			else if(sign_a =0 andalso sign_b =0 andalso is_a1_greater_than_b1 =true )
			then( str2bi(subst (a11,b11,hd(tl(rev a11)),hd((rev a11)),hd((rev b11)),0,0 ,"") ))
			else if(sign_a =0 andalso sign_b =0 andalso is_a1_greater_than_b1 =false )
			then(str2bi( "-" ^ subst (b11,a11,hd(tl(rev b11)),hd((rev b11)),hd((rev a11)),0,0 ,"")) )
			else if(sign_a =0 andalso sign_b =1 andalso is_a1_greater_than_b1 =true )
			then( (addeqlen a1 b1 0 [] )@[0])
			else if(sign_a =0 andalso sign_b =1 andalso is_a1_greater_than_b1 =false )
			then( (addeqlen a1 b1 0 [] )@[0])
			else if(sign_a =1 andalso sign_b =0 andalso is_a1_greater_than_b1 =true )
			then( (addeqlen a1 b1 0 [] )@[1])
			else if(sign_a =1 andalso sign_b =0 andalso is_a1_greater_than_b1 =false )
			then( (addeqlen a1 b1 0 [] )@[1])
			else if(sign_a =1 andalso sign_b =1 andalso is_a1_greater_than_b1 =true )
			then(str2bi("-"^ subst (a11,b11,hd(tl(rev a11)),hd((rev a11)),hd((rev b11)),0,0 ,"")) )
			else if(sign_a =1 andalso sign_b =1 andalso is_a1_greater_than_b1 =false )
			then(str2bi( subst (b11,a11,hd(tl(rev b11)),hd((rev b11)),hd((rev a11)),0,0 ,""))) (**)
			else []; 
		fun remove_leading_zeros x = if (hd(x)=0 andalso tl(x)<>[0]) then (remove_leading_zeros (tl(x))) else x;
		val stg2 = remove_leading_zeros stg1;
		
	in(stg2)
	end;

fun div4bigint (a,b) = 
	let 
		val sign_a = hd(rev(a));
		val sign_b = hd(rev(b));
		fun divis (x ,y ,z) = 
			if (geq (x,y)) 
			then( divis ((sub(x,y)) ,y ,(add ([1,0],z)) )) 
			else z;
		fun division (res , a,b,c,divd,modu,count) = 
			if(b=[0,0] orelse b = [0,1])
			then raise Divbyzero
			else if (count <> length a)
			then (division (res^bi2str(divd),
				str2bi(bi2str a),
				str2bi(bi2str b), 
				
				str2bi(bi2str (add(([List.nth(a,count)]@[0]),
				(str2bi(bi2str(mul(str2bi(bi2str(sub(c,  
				str2bi(bi2str (mul(b,str2bi(
				bi2str (divis (str2bi(bi2str c),b,[0,0]))))) )  ))) ,[1,0,0]))))))) ,
				
				str2bi(bi2str(divis(c,b,[0,0]))), 
				
				str2bi(bi2str(sub(c,  str2bi(bi2str (mul(b,str2bi(bi2str (divis (str2bi(bi2str c),b,[0,0]))))) )  ))) ,
				count+1  ))
			else (str2bi(bi2str(str2bi(res^bi2str(divd)))));
		val abs_a = if(sign_a = 0) then a else unminus(a);
		val abs_b = if(sign_b = 0) then b else unminus(b);
		val abs_res = division("", abs_a, abs_b ,([hd abs_a] @ [0]) ,[0,0],[0,0],1);
		val stg1 = 
			if(sign_a = 0 andalso sign_b = 0) orelse (sign_a = 1 andalso sign_b = 1)
			then (abs_res )
			else (unminus(abs_res));
	in(stg1 )
	end
and mul (a,b) = 
	let
		fun generate_continuous_list_post str num outp = 
			if (num = 0) then outp else generate_continuous_list_post (str) (num - 1) (outp@[str]);
		val sign_a = hd(rev(a));
		val sign_b = hd(rev(b));
		val sign = sign_a + sign_b;
		val abs_a = if(sign_a = 0) then a else unminus(a);
		val abs_b = if(sign_b = 0) then b else unminus(b);
		val mag_a = rev(tl(rev(a)));
		val mag_b = rev(tl(rev(b)));
		fun mul_by_single_digit x y z = if(gt(y,[1,0])) then (mul_by_single_digit (add (x,z)) (sub(y,[1,0])) z) else (if(y=[0,0] orelse y = [0,1])then[0,0]else x); 
		fun mult (a ,b) cnt c res = 
			if ((length b -2) <> cnt)
			then ( mult (a ,b) (cnt+1) ([List.nth((rev b),cnt+2)]@[0]) (add(res,(generate_continuous_list_post 0 (cnt) (mul_by_single_digit a c a) ) ))  )
			else (add(res,(generate_continuous_list_post 0 (cnt) (mul_by_single_digit a c a) ) ));
		val v = mult (abs_a ,abs_b) 0 (List.drop(abs_b,length abs_b -2)) [0,0];
		val v2 = if(sign = 1) then (List.take(v,length v -1)@[1]) else v;
	in( v2)
	end;
fun mod4bigint (a,b) = 
	let 
		val sign_a = hd(rev(a));
		val sign_b = hd(rev(b));
		fun modul x y = if (geq (x,y)) then( modul (sub(x,y)) y) else x;
		fun divis (x ,y ,z) = 
			if (geq (x,y)) 
			then( divis ((sub(x,y)) ,y ,(add ([1,0],z)) )) 
			else z;
		fun division (res , a,b,c,divd,modu,count) = 
			if(b=[0,0] orelse b = [0,1])
			then raise Divbyzero
			else if (count <> length a)
			then (division (res^bi2str(divd),
				str2bi(bi2str a),
				str2bi(bi2str b), 
				
				str2bi(bi2str (add(([List.nth(a,count)]@[0]),
				(str2bi(bi2str(mul(str2bi(bi2str(sub(c,  
				str2bi(bi2str (mul(b,str2bi(
				bi2str (divis (str2bi(bi2str c),b,[0,0]))))) )  ))) ,[1,0,0]))))))) ,
				
				str2bi(bi2str(divis(c,b,[0,0]))), 
				
				str2bi(bi2str(sub(c,  str2bi(bi2str (mul(b,str2bi(bi2str (divis (str2bi(bi2str c),b,[0,0]))))) )  ))) ,
				count+1  ))
			else (str2bi(bi2str modu));
		val abs_a = if(sign_a = 0) then a else unminus(a);
		val abs_b = if(sign_b = 0) then b else unminus(b);
		val abs_res = division("", abs_a, abs_b ,([hd abs_a] @ [0]) ,[0,0],[0,0],1);;
		val stg1 = 
			if(sign_a = 0 )
			then (abs_res )
			else (unminus(abs_res));
	in(stg1 )
	end;	
end;
