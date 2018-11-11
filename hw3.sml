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

