(* 1 *)
fun is_older(date1 : int * int * int, date2 : int * int * int) = 
    (#1 date1) < (#1 date2) 
    orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)
(* 2 *)
fun number_in_month(date : (int * int * int) list, month : int) = 
    if null date
    then 0
    else let val tmp = number_in_month(tl date, month)
         in
             if (#2 (hd date)) = month
             then tmp + 1
             else tmp
         end
(* 3 *)
fun number_in_months(date : (int * int * int) list, month : int list) =
    if null month
    then 0
    else number_in_months(date, tl month) + number_in_month(date, hd month)
(* 4 *)
fun dates_in_month(date : (int * int * int) list, month : int) = 
    if null date
    then []
    else let val tmp = dates_in_month(tl date, month)
         in 
             if (#2 (hd date)) = month
             then (hd date)::tmp
             else tmp
         end
(* 5 *)
fun dates_in_months(date : (int * int * int) list, month : int list) = 
    if null month
    then []
    else dates_in_month(date, hd month)@dates_in_months(date, tl month)
(* 6 *)
fun get_nth(date : string list, n : int) = 
    if null date orelse n <= 0
    then ""
    else if n = 1
    then hd date
    else get_nth(tl date, n - 1)
(* 7 *)
fun date_to_string(year : int, month : int, day : int) = 
    let val s = ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
    in
        get_nth(s, month) ^ Int.toString(day) ^ ", " ^ Int.toString(year)   
    end
(* 8 *)
fun number_before_reaching_sum(sum : int, a : int list) = 
    if sum <= 0
    then ~1
    else number_before_reaching_sum(sum - (hd a), tl a) + 1
(* 9 *)
fun what_month(day : int) = 
    let val a = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, a) + 1
    end
(* 10 *)
fun month_range(day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)
(* 11 *)
fun oldest(date : (int * int * int) list) = 
    if null date
    then NONE
    else let val tmp = oldest(tl date)
         in 
            if isSome(tmp) andalso is_older(valOf(tmp), hd date)
            then tmp
            else SOME(hd date)
         end
(* 12 *)
fun remove_duplicates(x : int list) = 
    if List.length(x) <= 1
    then x
    else let val tmp = remove_duplicates(tl x)
         in let 
                fun element_in_list(number : int, tmp : int list) = 
                   if null tmp
                   then false
                   else let val eq = element_in_list(number, tl tmp)
                        in 
                            if eq orelse (hd tmp) = number
                            then true
                            else false
                        end
            in
                if element_in_list(hd x, tmp)
                then tmp
                else (hd x)::tmp
            end
        end

fun number_in_months_challenge(date : (int * int * int) list, month : int list) = 
    number_in_months(date, remove_duplicates(month))

fun dates_in_months_challenge(date : (int * int * int) list, month : int list) = 
    dates_in_months(date, remove_duplicates(month))
(* 13 *)
fun reasonable_date(date : int * int * int) = 
    let 
        val a = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val year = (#1 date)
        val month = (#2 date)
        val day = (#3 date)
    in
        if year <= 0
        then false
        else
            if month < 1 orelse month > 12
            then false
            else
                if month <> 2 
                then if day >= 1 andalso day <= List.nth(a, month - 1)
                     then true
                     else false
                else if (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0) 
                     then (day >= 1 andalso day <= 29)
                     else (day >= 1 andalso day <= 28)
    end