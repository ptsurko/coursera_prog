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
