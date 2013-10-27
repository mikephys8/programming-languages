(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "homework-3.sml";
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["Arc","Bat","Can"] = ["Arc","Bat","Can"]
val test1b = only_capitals ["Arc","bat","Can"] = ["Arc","Can"]
val test1c = only_capitals ["arc","bat","can"] = []

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 ["A","bc","cc"] = "bc"
val test2b = longest_string1 ["Apple","bc","cc"] = "Apple"
val test2c = longest_string1 ["Apple","bc","coconut"] = "coconut"
val test2d = longest_string1 [] = ""

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["A","bc","cc"] = "cc"
val test3b = longest_string2 ["Apple","bc","cc"] = "Apple"
val test3c = longest_string2 ["Apple","bc","coconut"] = "coconut"
val test3d = longest_string2 [] = ""

val test4a= longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string3 ["A","bc","cc"] = "bc"
val test4c = longest_string3 ["Apple","bc","cc"] = "Apple"
val test4d = longest_string3 ["Apple","bc","coconut"] = "coconut"
val test4e = longest_string3 [] = ""

val test4f = longest_string4 ["A","B","C"] = "C"
val test4g = longest_string4 ["A","bc","C"] = "bc"
val test4h = longest_string4 ["A","bc","cc"] = "cc"
val test4i = longest_string4 ["Apple","bc","cc"] = "Apple"
val test4j = longest_string4 ["Apple","bc","coconut"] = "coconut"
val test4k = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A";
val test5a = longest_capitalized ["a","bc","cdfjioaf"] = "";
val test5b = longest_capitalized [] = "";
val test5c = longest_capitalized ["A","bc","Cat"] = "Cat";

val test6 = rev_string "abc" = "cba";
val test6a = rev_string "a" = "a";
val test6b = rev_string "" = "";
val test6c = rev_string "Foo" = "ooF";

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test8b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (TupleP [Variable("a"), Wildcard, Variable("blah")]) = 6

val test9c = count_some_var ("x", Variable("x")) = 1;
val test9c1 = count_some_var ("x", TupleP [Variable("x"), Variable("y"), Variable("x"), Variable("xyz")]) = 2;

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP [Variable("x"), Variable("xy")]) = true
val test10b = check_pat (Wildcard) = true
val test10c = check_pat (TupleP [Variable("x"), Variable("x")]) = false

(* val test11 = match (Const(1), UnitP) = NONE *)

(* val test12 = first_match Unit [UnitP] = SOME [] *)

