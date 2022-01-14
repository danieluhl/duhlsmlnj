(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

	     (* remove first instance of a string from a list *)
fun all_except_option (str, strList) =
    case strList of
	[] => NONE
      | x::xs => if
		    same_string(x, str) = true
		then
		    SOME xs
		else
		    case all_except_option(str, xs) of
			NONE => NONE
		      | SOME(lst) => SOME(x::lst)

fun get_substitutions1 (wordList, word) =
    case wordList of
	[] => []
      | x::xs => let val nextSubs = get_substitutions1(xs, word)
		 in
		     case all_except_option(word, x) of
			 NONE => nextSubs
		       | SOME(lst) => lst @ nextSubs
		 end

fun get_substitutions2 (words_lists, word) = 
    let fun f (words_lists, word, acc) =
	    case words_lists of
		[] => acc
	      | x::xs => case all_except_option(word, x) of
			     NONE => f(xs, word, acc)
			   | SOME(lst) => f(xs, word, lst @ acc)
    in
	f(words_lists, word, [])
    end

fun similar_names(names_lists, fullName) =
    case fullName of
	{first, middle, last} => (case get_substitutions1(names_lists, first) of
				      [] => fullName::[]
				    | names => let fun f (firsts, middle, last) =
						       case firsts of
							   [] => []
							 | first::firsts' => {first=first, middle=middle, last=last}::f(firsts', middle, last)
					       in
						   fullName::f(names, middle, last)
					       end
				 )		 

			 

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      (* put your solutions for problem 2 here *)
fun card_color c =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

fun card_value c =
    case c of
	(_, Ace) => 11
      | (_, Jack) => 10
      | (_, King) => 10
      | (_, Queen) => 10
      | (_, Num (i)) => i

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::xs => if c = x
		 then
		     xs
		 else
		     x::remove_card(xs, c, e)

fun all_same_color(cs) =
    case cs of
	[] => true
      | c::[] => true
      | c1::c2::cx => if card_color c1 = card_color c2
		      then
			  all_same_color(c2::cx)
		      else
			  false
	

fun sum_cards(cs) =
    let fun f (cs, acc) =
	    case cs of
		[] => acc
	      | c::cs' => f(cs', acc+card_value(c))
    in
	f(cs, 0)
    end

fun score(cs, goal) =
    let
	val sum = sum_cards(cs)
	val all_same = all_same_color(cs)
	val card_score = 
	    if sum < goal
	    then
		goal - sum
	    else
		3 * (sum - goal)
    in
	if all_same
	then
	    card_score div 2
	else
	    card_score
    end
	
fun officiate (cs, ms, goal) =
    let fun f(hcs, cs, ms, hcs_sum) =
	    case ms of
		[] => score(hcs, goal)
	      | Discard(c)::ms' => f(remove_card(hcs, c, IllegalMove), cs, ms', hcs_sum - card_value(c))
	      | Draw::ms' => case cs of
				 [] => score(hcs, goal)
			       | c::cs' => let
				   val next_hcs_sum = hcs_sum + card_value(c)
				   val next_hcs = c::hcs
			       in
				   if next_hcs_sum > goal
				   then
				       score(next_hcs, goal)
				   else
				       f(next_hcs, cs', ms', hcs_sum + card_value(c))
			       end
    in
	f([], cs, ms, 0)
    end
		  
