fun is_older(d1 : int*int*int, d2 : int*int*int) =
	(#1 d1) < (#1 d2) orelse (#2 d1) < (#2 d2) orelse (#3 d1) < (#3 d2);

fun number_in_month(dates: (int*int*int) list, month: int) = 
	if null dates
	then 0
	else if (#2 (hd dates)) = month
	then 1 + number_in_month(tl dates, month)
	else 0 + number_in_month(tl dates, month);

fun number_in_months(dates: (int*int*int) list, monthes: int list) = 
	if null dates
	then 0
	else
		let
	    	fun in_list(month: int, monthes: int list) = 
	    		if null monthes
	    		then 0
	    		else if month = (hd monthes)
	    		then 1
	    		else in_list(month, tl monthes)
		in
	    	in_list(#2 (hd dates), monthes) + number_in_months(tl dates, monthes) 
		end