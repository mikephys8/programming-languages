(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "homework-1.sml";

val test1 = is_older((1,2,3),(2,3,4)) = true

val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3a = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = 4

val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"
val test7a = date_to_string((5, 2, 28)) = "February 28, 5"
val test7b = date_to_string((199, 1, 1)) = "January 1, 199"
val test7c = date_to_string((2010, 12, 31)) = "December 31, 2010"

val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test8a = number_before_reaching_sum(6, [1,2,3,4,5]) = 2
val test8b = number_before_reaching_sum(3, [1,2,3,4,5]) = 1
val test8c = number_before_reaching_sum(2, [1,2,3,4,5]) = 1
val test8d = number_before_reaching_sum(0, [1,2,3,4,5]) = 0

val test9 = what_month(70) = 3
val test9a = what_month(1) = 1
val test9b = what_month(31) = 1
val test9c = what_month(32) = 2
val test9d = what_month(365) = 12

val test10 = month_range(31, 34) = [1,2,2,2]
val test10a = month_range(1, 1) = [1]
val test10b = month_range(59, 60) = [2,3]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11a = oldest([]) = NONE
val test11b = oldest([(1,2,3),(4,5,6),(7,8,9)]) = SOME (1,2,3)
val test11c = oldest([(2011,3,31)]) = SOME (2011,3,31)
val test11d = oldest([(2012,2,28),(2011,3,31),(2011,4,28),(2011, 2, 28)]) = SOME (2011,2,28)

val test12 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = 3
val test12a = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,3,3,4,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test_remove_duplicates = remove_duplicates([]) = []
val test_remove_duplicates_a = remove_duplicates([1,2,3]) = [1,2,3]
val test_remove_duplicates_b = remove_duplicates([1]) = [1]
val test_remove_duplicates_c = remove_duplicates([1,1,1,1,1,1]) = [1]
val test_remove_duplicates_d= remove_duplicates([1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6]) = [1,2,3,4,5,6]
val test_remove_duplicates_e= remove_duplicates([1,2,3,3,3,3,3,3,3,3,3]) = [1,2,3]

val test13 = reasonable_date((2012,1,31))
val test13a = reasonable_date((2,2,28))
val test13b = reasonable_date((2000,2,29))
val test13c = reasonable_date((2001,2,29)) = false
val test13d = reasonable_date((2001,14,29)) = false
val test13e = reasonable_date((~2001,1,29)) = false
val test13f = reasonable_date((2001,14,50)) = false
