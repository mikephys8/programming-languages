(* 1. Write a function is_older that takes two dates and evaluates to true or false.
 It evaluates to true if the first argument is a date that comes before the second argument.
 (If the two dates are the same, the result is false.) *)
fun is_older (date_1 : (int * int * int), date_2 : (int * int * int)) =
    if date_1 = date_2 then false
    else if #1 date_1 < #1 date_2
    then true
    else if #1 date_1 > #1 date_2
    then false
    else if #2 date_1 < #2 date_2
    then true
    else if #2 date_1 > #2 date_2
    then false
    else #3 date_1 < #3 date_2


(* 2. Write a function number_in_month that takes a list of dates and a month
 (i.e., an int) and returns how many dates in the list are in the given month. *)
fun number_in_month (dates : ((int * int * int) list), month : int) =
    if null dates then 0
    (* TODO: use let to only call number_in_month once *)
    else if (#2 (hd dates)) = month then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

(* 3. Write a function number_in_months that takes a list of dates and a list of
 months (i.e., an int list) and returns the number of dates in the list of dates
 that are in any of the months in the list of months. Assume the list of months
 has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4. Write a function dates_in_month that takes a list of dates and a month
 (i.e., an int) and returns a list holding the dates from the argument list of
 dates that are in the month. The returned list should contain dates in the order
 they were originally given. *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates then []
    else if (#2 (hd dates)) = month then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(* 5. Write a function dates_in_months that takes a list of dates and a list of
 months (i.e., an int list) and returns a list holding the dates from the argument
 list of dates that are in any of the months in the list of months. Assume the list
 of months has no number repeated. Hint: Use your answer to the previous problem
 and SML's list-append operator (@). *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null months then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
