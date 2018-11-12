(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
    List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs


fun longest_string2 xs =
    List.foldl (fn (x,y) => if String.size x >= String.size y then x else y)
	       ""  xs
	       

fun longest_string_helper f xs =
    case xs of
	[] => ""
      | x::xs' =>
	let
	    val ans = longest_string_helper f xs'
	in
	    if f(String.size(ans),String .size(x))
	    then ans
	    else x
	end
	    
		
val longest_string3 = longest_string_helper (fn (a,b) => a > b)
val longest_string4 = longest_string_helper (fn (a,b) => a >= b)


val longest_capitalized = longest_string3 o only_capitals 
					    
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME y => y
				    
fun all_answers f xs =
    let
	fun helper (acc, xs) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME y => helper(acc@y, xs')
    in
	helper ([], xs)
    end

val rev_string = String.implode o List.rev o String.explode

fun count_wildcards p = g (fn () => 1) (fn x => 0) p
    

fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size x) p
      
			  
fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x=s then 1 else 0) p

fun check_pat p =
    let
	fun helper1 p =
	    case p of
		TupleP ps =>List.foldl (fn (a,b) => (helper1 a)@b) [] ps
				(* 这里可以替换成 List.foldl (fn (a,b) => (helper1 a)@b)  [] ps *)
	      | Variable s => [s]
	      | ConstructorP(_,p) => helper1 p
	      | _ => []
			 
	fun helper2 lst =
	    case lst of
		[] => true
	      | x::xs' => (not (List.exists (fn s => x=s) xs')) andalso (helper2 xs')

    in
	helper2 (helper1 p)
    end
	

fun match (v, p) =
    case (v,p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const a, ConstP b) => if a=b then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if (length vs = length ps)
				 then all_answers match (ListPair.zip(vs,ps))
				 else NONE
      | (Constructor(s2,v'), ConstructorP(s1,p')) => if s1=s2
						     then match(v',p')
						     else NONE
      | _ => NONE


fun first_match v  ps  =
    SOME(first_answer (fn x=>match(v,x)) ps) handle NoAnswer => NONE
	    
      
fun reduce f acc xs =
    case xs of
	[] => acc
      | x::xs' => reduce f (f(x,acc)) xs'
			 
