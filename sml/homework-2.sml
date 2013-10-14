(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1. This problem involves using first-name substitutions to come up with alternate
 names. For example, Fredrick William Smith could also be Fred William Smith or Freddie
 William Smith. Only part (d) is specifically about this, but the other problems are helpful. *)

(* (a) Write a function all_except_option, which takes a string and a string list.
 Return NONE if the string is not in the list, else return SOME lst where lst is
 identical to the argument list except the string is not in it. You may assume the
 string is in the list at most once. Use same_string, provided to you, to compare
 strings. Sample solution is around 8 lines. *)

fun all_except_option (str1 : string, str_list : string list) =
    let fun remove_from_list (str_to_remove, remaining_list) =
        case remaining_list of
            [] => []
          | x :: x' => if same_string(x, str_to_remove) then x'
                       else x :: remove_from_list(str_to_remove, x')
        val filtered_list = remove_from_list(str1, str_list)
    in
        if filtered_list = str_list then NONE else SOME filtered_list
    end

(* (b) Write a function get_substitutions1, which takes a string list list
 (a list of list of strings, the substitutions) and a string s and returns a string list.
 The result has all the strings that are in some list in substitutions that also has s,
 but s itself should not be in the result. Example:
    get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
    (* answer: ["Fredrick","Freddie","F"] *)
Assume each list in substitutions has no repeats. The result will have repeats if
 s and another string are both in more than one list in substitutions. Example:
    get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")
    (* answer: ["Jeffrey","Geoff","Jeffrey"] *)
Use part (a) and ML's list-append (@) but no other helper functions. Sample solution
 is around 6 lines. *)
fun get_substitutions1 (substitutions : string list list, s : string) =
    let fun get_substitutions (list_of_lists : string list list ) =
            case list_of_lists of
                [] => []
                   | x::x' => let val result = all_except_option(s,x) in
                                  case result of
                                      NONE => get_substitutions(x')
                                   | SOME str_list => str_list @ get_substitutions(x')
                              end
    in
        get_substitutions(substitutions)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
