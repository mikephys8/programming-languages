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

    let fun exists (check_string : string, remaining_list: string list) =
        case remaining_list of
            [] => false
         | x::x' => if same_string(x, check_string) then true
                    else exists(check_string, x')
    in
        if exists(str1, str_list) then
            let fun remove_from_list (str_to_remove, remaining_list) =
                case remaining_list of
                    [] => []
                 | x :: x' => if same_string(x, str_to_remove) then x'
                              else x :: remove_from_list(str_to_remove, x')
            in
                SOME (remove_from_list(str1, str_list))
            end
        else NONE
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
