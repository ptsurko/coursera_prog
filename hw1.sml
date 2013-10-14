fun is_older(d1 : int*int*int, d2 : int*int*int) =
	(((#1 d1) * 1000 - (#1 d2) * 1000) + ((#2 d1) * 10 - (#2 d2) * 10) + ((#3 d1) - (#3 d2))) < 0
	
fun number_in_month(dates: (int*int*int) list, month: int) = 
	if null dates
	then 0
	else if (#2 (hd dates)) = month
	then 1 + number_in_month(tl dates, month)
	else 0 + number_in_month(tl dates, month);

fun number_in_months(dates: (int*int*int) list, monthes: int list) = 
	if null dates
	then 0
	else if null monthes
	then 0
	else number_in_month(dates, hd monthes) + number_in_months(dates, tl monthes)

fun dates_in_month(dates: (int*int*int) list, month: int) =
	if null dates
	then []
	else if (#2 (hd dates)) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month);

fun dates_in_months(dates: (int*int*int) list, monthes: int list) = 
	if null dates
	then []
	else if null monthes
	then []
	else dates_in_month(dates, hd monthes) @ dates_in_months(dates, tl monthes);

fun get_nth(values: string list, index: int) = 
	let
	    fun next(values: string list, current_index: int) = 
	    	if current_index = index
	    	then hd values
	    	else next(tl values, current_index + 1)
	in
	    next(values, 1)
	end


fun date_to_string(date: int*int*int) = 
	let
	    val monthes = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
	    fun find_month(monthes: string list, index: int) =
	    	if null monthes
	    	then ""
	    	else if index = (#2 date)
	    	then hd monthes
	    	else find_month(tl monthes, index + 1)
	in
	    find_month(monthes, 1) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end


fun number_before_reaching_sum(sum: int, numbers: int list) =
	let
	    fun sum_with_next(sum_internal: int, prev_value: int, numbers: int list) =
	    	if null numbers
	    	then prev_value
	    	else if (sum_internal + (hd numbers)) >= sum
	    	then prev_value + 1
	    	else sum_with_next(sum_internal + (hd numbers), prev_value + 1, tl numbers)	
	in
	    sum_with_next(hd numbers, 0, tl numbers)
	end


fun what_month(day: int) = 
	let
	    val monthes = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    fun sum(sum_internal: int, index: int, numbers: int list) =
	    	if sum_internal + hd numbers >= day
	    	then index
	    	else sum(sum_internal + hd numbers, index + 1, tl numbers)
	    in
	    sum(0, 1, monthes)
	end

fun month_range(day1: int, day2: int) = 
	if day1 > day2 
	then []
	else let
	    fun move_to_next_day(current_day: int) = 
	    	if current_day = day2
	    	then what_month(day2) :: []
	    	else what_month(current_day) :: move_to_next_day(current_day + 1)
	in
	    move_to_next_day(day1)
	end

fun oldest(dates: (int*int*int) list) = 
	if null dates
	then NONE
	else let
	    fun get_older_date(oldest: int*int*int, dates: (int*int*int) list) =
	    	if null dates
	    	then SOME oldest
	    	else if is_older(oldest, hd dates)
	    	then get_older_date(oldest, tl dates)
	    	else get_older_date(hd dates, tl dates)
	in
	    get_older_date(hd dates, tl dates)
	end
