(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, lst) =
    case lst of
	[] => NONE
      | x::xs => if same_string(s, x)
		 then SOME xs
		 else
		     case all_except_option (s, xs) of
			 NONE => NONE
		       | SOME i => SOME (x::i)


(*
fun get_substitutions1 (lst, s) =
    case lst of
	[] => []
      | x::xs =>
	let
	    val ans = get_substitutions1(xs, s)
	in
	    case all_except_option (s, x) of
		NONE => ans
	      | SOME i => i @ ans
	end   *)

	    
fun get_substitutions1(lst,s)=
    case lst of
    []=>[]
      | x::xs =>case all_except_option(s,x) of
            NONE=>get_substitutions1(xs,s)
		 | SOME i =>i @ get_substitutions1(xs,s)


fun get_substitutions2(lst, s) =
    let
	fun aux (lst, acc) =
	    case lst of
		[] => acc
	      | x::xs => case all_except_option (s, x) of
			     NONE => aux (xs, acc)
			   | SOME i => aux (xs, acc @ i)
    in
	aux(lst, [])
    end
	
fun similar_names (strs, name) =
    let
	val {first=f, middle=m, last=l} = name  (* val 的模式匹配 *)
	fun helper (lst) =
	    case lst of
		[] => []
	      | x::xs => {first=x,middle=m,last=l} :: helper(xs)
    in
	helper(f::get_substitutions2(strs, f))
    end
	
	
					      

						  

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color card =
    case card of
	(Spades, _) => Black
      | (Clubs, _) => Black
      | (_, _) => Red

fun card_value card =
    case card of
	(_, Num i) => i
      | (_, Ace) => 11
      | (_, _) => 10



(* 这不就是我们想要的吗?,只是去除第一个找到的 
  虽然递归一般来说是先求值后面的结果，再依次回退，但是我们可以设置一些if else 条件中途就截止 
  突然想到这个不就是平时我在做的事情吗？难道就是因为这可能是一个中途结束的递归我就反应不过来了吗？
  还是因为之前的思想禁锢了，没有反应过来？ *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::xs => if x=c
		 then xs
		 else x::remove_card(xs, x, e)


fun all_same_color cs =
    case cs of
	[] => true
      | [_] => true
      | a::b::c => (* at least two element *)
	 card_color(a)=card_color(b) andalso all_same_color(b::c)
		 
				    
fun sum_cards cs =
    let
	fun helper (cs, acc) =
	    case cs of
		[] => acc
	      | x::xs => helper (xs, acc + card_value x)
    in
	helper (cs, 0)
    end

fun score (cs, goal) =
    let
	val sum = sum_cards cs
	val s = if sum > goal then 3*(sum-goal) else (goal-sum)
						  
    in
	if all_same_color cs
	then s div 2
	else s
    end


fun officiate (cs, ms, goal) =
    let
 	fun continue (cs, hs, ms, goal) =
	    case ms of
		[] => score (hs, goal)
	      | Draw::ms'=>(case cs of
				[] => score (hs, goal)
			      | card::cs' => if sum_cards (card::hs) > goal
					     then score (card::hs, goal)
					     else continue (cs', card::hs, ms', goal))
	      | (Discard c)::ms' => continue (cs, remove_card(hs, c, IllegalMove), ms', goal)
    in
	continue (cs, [], ms, goal)
    end
