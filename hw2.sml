(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

(* put your solutions for problem 1 here *)
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
	let 
		fun get_substitutions(names: string list list, except: string, accumulator: string list) = 
			case names of
				  [] => accumulator
				| x::xs' => case all_except_option(except, x) of
								  NONE => get_substitutions(xs', except, accumulator)
								| SOME l => get_substitutions(xs', except, l @ accumulator)
	in 
		get_substitutions(names, except, [])
	end

fun similar_names(names: string list list, substitute: {first:string,middle:string,last:string}) =
	let 
		fun traverse(names: string list, last: string, middle: string) =
			case names of
				  [] => []
				| x::xs' => {first=x,last=last, middle=middle}::traverse(xs', last, middle)
	in 
		case get_substitutions1(names, #first substitute) of 
			  [] => [substitute]
			| x::xs' => substitute::traverse(x::xs', #last substitute, #middle substitute)
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

fun card_color(c: card) = 
	case #1 c of
		  Clubs => Black
		| Spades => Black
		| Diamonds => Red
		| Hearts => Red

fun card_value(c: card) =
	case #2 c of 
		  Num x => x
		| Jack => 10
		| Queen => 10
		| King => 10
		| Ace => 11

fun remove_card(cards: card list, cardToRemove: card, ex) =
	case cards of
		  [] => raise ex
		| x::xs' => if (#1 x) = (#1 cardToRemove) andalso (#2 x) = (#2 cardToRemove) 
				   then xs'
				   else x::remove_card(xs', cardToRemove, ex)


fun all_same_color(cards: card list) =
	case cards of
		  [] => true
		| x::xs' => case xs' of
						  [] => true
						| y::ys' => if card_color(x) = card_color(y)
									then all_same_color(xs')
									else false

fun sum_cards(cards: card list) = 
	let
		fun sum(cards: card list, accumulator: int) =
			case cards of
				  [] => accumulator
				| x::xs' => sum(xs', card_value(x) + accumulator)  
	in
		sum(cards, 0)
	end


fun score(cards: card list, goal: int) =
	let
		val sum = sum_cards(cards)
	in

		if sum < goal
		then if all_same_color(cards)
			 then (goal - sum) div 2
			 else (goal - sum)
		else if all_same_color(cards)
			then ((sum - goal) * 3) div 2
			else (sum - goal) * 3
	end


fun officiate(cards: card list, moves: move list, goal: int) =
	let
		fun move(cards: card list, hand: card list, moves: move list, goal: int) =
			case moves of
				  [] => score(hand, goal)
				| x::xs' => case x of
								  Discard c => move(cards, remove_card(hand, c, IllegalMove), xs', goal)
								| Draw => case cards of
										  [] => score(hand, goal)
										| y::ys' => if sum_cards(y::hand) > goal
													then score(y::hand, goal)
													else move(ys', y::hand, xs', goal)
	in
		move(cards, [], moves, goal)
	end