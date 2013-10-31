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


fun only_capitals(strings: string list) = 
	List.filter (fn x => Char.isUpper(String.sub(x, 0))) strings


fun longest_string1(strings: string list) =
	let
		fun fold(f, acc, l) =
			case l of
				  [] => acc
				| x::xs' => fold(f, f(acc, x), xs')		
	in
		case strings of
			  [] => ""
			| x::xs' => fold((fn (acc, s) =>
										 if String.size(acc) < String.size(s)
										 then s
										 else acc),
							 x, xs')
	end


fun longest_string2(strings: string list) =
	let
		fun fold(f, acc, l) =
			case l of
				  [] => acc
				| x::xs' => fold(f, f(acc, x), xs')		
	in
		case strings of
			  [] => ""
			| x::xs' => fold((fn (acc, s) =>
										 if String.size(acc) <= String.size(s)
										 then s
										 else acc),
							 x, xs')
	end

fun longest_string22(strings: string list) =
	let
		fun foldr(f, acc, l) = 
			case l of
				  [] => acc
				| x::xs' => f(foldr(f, acc, xs'), x)
	in
		case strings of
			  [] => ""
			| x::xs' => foldr((fn (acc, s) =>
										 if String.size(acc) < String.size(s)
										 then s
										 else acc),
							"", strings)
	end

fun longest_string_helper f strings =
	let
		fun fold(f, acc, l) =
			case l of
				  [] => acc
				| x::xs' => fold(f, f(acc, x), xs')
	in
		case strings of
			  [] => ""
			| x::xs' => fold((fn (acc, s) => if f (String.size(s), String.size(acc))
											 then s
											 else acc),
							  x, strings)
	end

fun longest_string3(strings: string list) =
	longest_string_helper (fn (a, b) => if a > b then true else false) strings


fun longest_string4(strings: string list) =
	longest_string_helper (fn (a, b) => if a > b then true else false) strings

val longest_capitalized = longest_string1 o only_capitals 

val rev_string = String.implode o List.rev o String.explode

fun first_answer f l =
	case l of 
		  [] => raise NoAnswer
		| x::xs' => case f x of
						  SOME x => x
						| NONE => first_answer f xs' 
