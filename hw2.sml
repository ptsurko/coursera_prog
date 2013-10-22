(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s1: string, s2: string list) = 
	case s2 of 
		  [] => NONE
		| x::xs' => case same_string(s1, x) of
						  true => SOME xs'
						| false => case all_except_option(s1, xs') of
								  NONE => NONE
								| SOME xss' => SOME(x::xss')

fun get_substitutions1(names: string list list, except: string) = 
	case names of
		  [] => []
		| x::xs' => case all_except_option(except, x) of
						  NONE => get_substitutions1(xs', except)
						| SOME l => l @ get_substitutions1(xs', except)


fun get_substitutions2(names: string list list, except: string) = 
	case names of
		  [] => []
		| x::xs' => case all_except_option(except, x) of
						  NONE => get_substitutions2(xs', except)
						| SOME l => l @ get_substitutions2(xs', except)

fun similar_names(names: string list list, substitute: {first:string,middle:string,last:string}) =
	let 
		fun traverse(names: string list, last: string, middle: string) =
			case names of
				  [] => []
				| x::xs' => {first=x,last=last, middle=middle}::traverse(xs', last, middle)
	in 
		case get_substitutions1(names, #first substitute) of 
			  [] => []
			| x::xs' => substitute::traverse(x::xs', #last substitute, #middle substitute)
	end
(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
(* datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove *)

(* put your solutions for problem 2 here *)
