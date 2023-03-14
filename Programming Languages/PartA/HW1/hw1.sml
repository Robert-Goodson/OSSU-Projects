fun is_older ( first: int*int*int , second : int*int*int) =
    let
	val y1 = #1 first
	val y2 = #1 second
	val m1 = #2 first
	val m2 = #2 second
	val d1 = #3 first
	val d2 = #3 second
    in
	y1 < y2 orelse ( y1 = y2 andalso m1 < m2)
	                orelse (y1 = y2  andalso m1 = m2 andalso d1<d2)
    end
	
fun number_in_month (xs:( int*int*int) list, month: int) =
    if null xs
    then 0
    else if #2(hd xs) = month
	then 1 + number_in_month(tl xs, month)
    else number_in_month(tl xs, month)

fun number_in_months (dates:(int*int*int) list, months : int list) =
    if null months
    then 0
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates:(int*int*int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then
	hd dates :: dates_in_month(tl dates, month)
    else
	dates_in_month(tl dates, month)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

fun get_nth (str: string list, n : int) =
    if n = 1
    then  hd str
    else  get_nth(tl str, n-1)

fun date_to_string (date:(int*int*int) ) =
    let
	val months = ["January","February","March","April",
		         "May","June","July","August",
		         "September","October","Noveber","December"]
    in			 
	 get_nth(months, #2 date) ^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum:int, lst:int list) =
    let
	val total = 0
	fun helper (sum, lst, total) =
	    if hd lst < sum
	    then helper(sum-(hd lst), tl lst, total +1)
	    else total
    in
	helper(sum, lst, total)
    end

fun what_month (day:int) =
    let
	val number = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day, number)
    end
	
fun month_range (day1: int, day2:int)=
    if day1>day2
    then []
    else what_month(day1)::month_range(day1+1,day2)

fun oldest (dates: (int*int*int)list) =
    if null dates
    then NONE
    else let
	val old = oldest(tl dates)
    in
	if isSome old andalso is_older(valOf old, hd dates)
	then old
		 else SOME(hd dates)
    end

fun in_list (month, months) =
    not(null months) andalso (month = hd months) orelse in_list(month, tl months)

fun removeDup (months)=
    if null months
    then []
    else
	let
	    val tl_ans = removeDup(tl months)
	in
	    if in_list(hd months, tl_ans)
	    then tl_ans
	    else (hd months)::tl_ans
	end
	    
fun number_in_months_challenge (dates:(int*int*int) list, months: int list)=
    number_in_months (dates,removeDup(months))

fun dates_in_months_challenge (dates:(int*int*int)list, months: int list)=
    dates_in_months(dates, removeDup months)
			 
fun leapyear (year)=
    (year mod 400) = 0 orelse (year mod 4)=0 andalso not ((year mod 100)=0)
fun reasonable_date (date:(int*int*int))=
    let
	val y = #1 date
	val  m = #2 date
	val d = #3 date
    in
	(y>0) andalso
	(m > 0 andalso m <13)andalso
	(if(leapyear(y))
	 then (d>0 andalso d<29)
	 else (d>0 andalso d<28))
    end
	




	     
		       
		  

		
