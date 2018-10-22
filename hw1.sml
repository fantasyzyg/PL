






fun is_older (date1: int*int*int, date2: int*int*int) =
    if (#1 date1)<(#1 date2)
    then true
    else if (#1 date1)=(#1 date2) andalso (#2 date1)<(#2 date2)
    then true
    else if (#1 date1)=(#1 date2) andalso (#2 date1)=(#2 date2) andalso (#3 date1)<(#3 date2)
    then true
    else false


fun number_in_month ( l: (int*int*int) list, month: int) =
    if null l
    then 0
    else
	let val x = number_in_month ( tl l, month)
	in
	    if (#2 (hd l)) = month
	    then x+1
	    else x
	end
	    
fun number_in_months ( l: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month (l, hd months) + number_in_months (l, tl months)






fun dates_in_month ( l: (int*int*int) list, m: int) =
    if null l
    then []
    else if (#2 (hd l)) = m
    then ( hd l ) :: dates_in_month ( tl l, m )
    else dates_in_month ( tl l, m )
	
fun dates_in_months (l: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month (l, hd months) @ dates_in_months (l, tl months) (**list append**)
					       


fun get_nth ( strs: string list, n: int) =
    if n=1
    then hd strs
    else get_nth ( tl strs, n-1)



fun date_to_string (date: int*int*int) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^", " ^Int.toString(#1 date)
    end
	


fun number_before_reaching_sum (sum: int, nums: int list) =
    let
	fun helper ( nums: int list, tmp: int) =
	    if tmp+(hd nums) >= sum
	    then 0
	    else 1 + helper (tl nums, tmp+ (hd nums))
    in
	helper(nums, 0)
    end




fun what_month (day: int) =
    let
	val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum (day, days) + 1
    end
	
fun month_range (s: int, e: int) =
    if s > e
    then []
    else what_month(s) :: month_range(s+1, e)

				     
