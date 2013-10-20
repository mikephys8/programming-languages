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
    case substitutions of
        [] => []
           | x::x' => let val result = all_except_option(s,x) in
                          case result of
                              NONE => get_substitutions1(x', s)
                                   | SOME str_list => str_list @ get_substitutions1(x', s)
                      end

(* (c) Write a function get_substitutions2, which is like get_substitutions1 except
 it uses a tail-recursive local helper function.*)
fun get_substitutions2 (substitutions : string list list, s : string) =
    let fun get_substitutions (list_of_lists : string list list, matches_so_far : string list ) =
            case list_of_lists of
                [] => matches_so_far
                   | x::x' => let val result = all_except_option(s,x) in
                                  case result of
                                      NONE => get_substitutions(x', matches_so_far)
                                   | SOME str_list => get_substitutions(x', matches_so_far @ str_list)
                              end
    in
        get_substitutions(substitutions, [])
    end

(* (d) Write a function similar_names, which takes a string list list of substitutions
 (as in parts (b) and (c)) and a full name of type {first:string,middle:string,last:string}
 and returns a list of full names (type {first:string,middle:string,last:string} list)
. The result is all the full names you can produce by substituting for the rst name
 (and only the first name) using substitutions and parts (b) or (c). The answer should
 begin with the original name (then have 0 or more other names). Example:
    similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
    {first="Fred", middle="W", last="Smith"})
    (* answer: [{first="Fred", last="Smith", middle="W"},
    {first="Fredrick", last="Smith", middle="W"},
    {first="Freddie", last="Smith", middle="W"},
    {first="F", last="Smith", middle="W"}] *)
Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is
around 10 lines.
*)
fun similar_names (substitutions : string list list, {first=first, middle=middle, last=last} ) =
    let val first_names = first :: get_substitutions2(substitutions, first);
        fun helper (first_name_list : string list) =
            case first_name_list of
                [] => []
                   | x::x' => {first=x, middle=middle, last=last} :: helper(x')
    in
        helper(first_names)
    end

(* 2. you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(*  (a) Write a function card_color, which takes a card and returns its color
 (spades and clubs are black, diamonds and hearts are red).
 Note: One case-expression is enough. *)
fun card_color (card_suit, _) =
    case card_suit of
        Clubs => Black
     | Spades => Black
     | Hearts => Red
     | Diamonds => Red

(* (b) Write a function card_value, which takes a card and returns its value
 (numbered cards have their number as the value, aces are 11, everything else is 10).
 Note: One case-expression is enough.
*)
fun card_value (_, some_rank) =
    case some_rank of
        Jack => 10
     | Queen => 10
     | King => 10
     | Ace => 11
     | Num value => value

(* (c) Write a function remove_card, which takes a list of cards cs, a card c,
 and an exception e. It returns a list that has all the elements of cs except c.
 If c is in the list more than once, remove only the first one. If c is not in the list,
 raise the exception e. You can compare cards with =. *)
fun remove_card (cs : card list, c : card, e) =
    let fun filter_card_list (card_list : card list) =
        case card_list of
            [] => []
          | x :: x' => if x=c then x'
                       else x :: filter_card_list(x')
        val filtered_card_list = filter_card_list(cs)
    in
        if filtered_card_list = cs then raise e else filtered_card_list
    end

(* (d) Write a function all_same_color, which takes a list of cards and returns
 true if all the cards in the list are the same color. Hint: An elegant solution
 is very similar to one of the functions using nested pattern-matching in the lectures. *)
fun all_same_color (cards : card list) =
    case cards of
        [] => true
     | first :: [] => true
     | first :: second :: rest => if card_color(first) = card_color(second)
                                  then all_same_color(second :: rest)
                                  else false

(* (e) Write a function sum_cards, which takes a list of cards and returns the sum
 of their values. Use a locally defined helper function that is tail recursive.*)
fun sum_cards (card_list : card list) =
    let fun sum_remaining (remaining_list : card list, sum : int) =
            case remaining_list of
                [] => sum
             | x :: x' => sum_remaining(x', sum + card_value(x))
    in
        sum_remaining(card_list, 0)
    end

(* (f) Write a function score, which takes a card list (the held-cards) and an int
 (the goal) and computes the score as described above *)
fun score (held_cards : card list, goal : int ) =
    let val sum = sum_cards(held_cards)
        val prelim_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
        val same_color = all_same_color(held_cards)
    in
        if same_color then prelim_score div 2 else prelim_score
    end

(* (g) Write a function officiate, which "runs a game." It takes a card list
 (the card-list) a move list (what the player "does" at each point), and an int
 (the goal) and returns the score at the end of the game after processing (some or all of)
 the moves in the move list in order. Use a locally defined recursive helper function that
 takes several arguments that together represent the current state of the game.
 As described above:
 The game starts with the held-cards being the empty list.
 The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
 If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
 If the player draws and the card-list is (already) empty, the game is over.
 Else if drawing causes the sum of the held-cards to exceed the goal, the game is
 over (after drawing). Else play continues with a larger held-cards and a smaller card-list.
Sample solution for (g) is under 20 lines.) *)
fun officiate (card_list : card list, move_list : move list, goal : int) =
    let fun play(held_cards : card list, moves : move list, remaining_cards : card list) =
            case moves of
                [] => score(held_cards, goal)
                   | x::x' => case x of
                                  Discard card => play(remove_card(held_cards, card, IllegalMove), x', remaining_cards)
                               |  Draw => case remaining_cards of
                                              [] => score(held_cards, goal)
                                            | y::y' => let val new_held_cards = y::held_cards in
                                                           if sum_cards(new_held_cards) > goal then score(new_held_cards, goal)
                                                           else play(new_held_cards, x', y')
                                                       end
    in
        play([], move_list, card_list)
    end



(* 3. Challenge Problems:
(a) Write score_challenge and officiate_challenge to be like their non-challenge counterparts
 except each ace can have a value of 1 or 11 and score_challenge should always return the
 least (i.e., best) possible score. (Note the game-ends-if-sum-exceeds-goal rule should
 apply only if there is no sum that is less than the goal.) Hint: This is easier than
 you might think. *)
