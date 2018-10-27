(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1a *)
fun all_except_option(s, xs) =
    case xs of
        [] => NONE
      | x::xs' => if same_string(s, x) 
                  then SOME xs'
                  else case all_except_option(s, xs') of
                    NONE => NONE
                  | SOME y => SOME(x::y)
(* 1b *)
fun get_substitutions1(xs, s) =
    case xs of
	      [] => []
        | x::xs' => case all_except_option(s, x) of
		                 NONE => get_substitutions1(xs', s)
		               | SOME y => y @ get_substitutions1(xs', s)
(* 1c *)
fun get_substitutions2(xs, s) = 
    let fun helper(lst, ans) = 
        case lst of
                [] => ans
              | x::x' => case all_except_option(s, x) of
                                NONE => helper(x', ans)
                              | SOME y => helper(x', y @ ans)
    in
        helper(xs, [])
    end
(* 1d *)
fun similar_names(xs, {first, middle, last}) = 
    let fun helper(lst, ans) = 
        case lst of 
                [] => ans
              | lst::lst' => helper(lst', ans @ [{first = lst, middle = middle, last = last}])
    in 
        helper(first::get_substitutions2(xs, first), [])
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
(* 2 a*)
fun card_color(card) = 
    case card of 
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
(* 2b *)
fun card_value(card) = 
    case card of
        (_, Ace) => 11
      | (_, Num x) => x
      | _ => 10
(* 2c *)
fun remove_card(cs, c, e) = 
    case cs of
        [] => raise e
      | xs::xs' => if xs = c 
                   then xs'
                   else xs::remove_card(xs', c, e)
(* 2d *)
fun all_same_color(xs) = 
    case xs of 
            [] => true
          | [_] => true
          | a::b::c => card_color(a) = card_color(b) andalso all_same_color(b::c)
(* 2e *)
fun sum_cards(xs) = 
    let fun helper(lst, sum) = 
        case lst of
            [] => sum
          | x::x' => helper(x', sum + card_value(x))
    in
        helper(xs, 0)
    end
(* 2f *)
fun score (cs,goal) = 
    let 
        val sum = sum_cards cs
    in
        (if sum >= goal then 3 * (sum - goal) else goal - sum)
	      div (if all_same_color cs then 2 else 1)
    end
(* 2g *)
fun officiate(cards, moves, goal) =
  let
    fun helper(_, [], heldcards) = score(heldcards, goal)
        | helper(cards, Discard card::moves', heldcards) = helper(cards, moves', remove_card(heldcards, card, IllegalMove))
        | helper([], Draw::moves', heldcards) = score(heldcards, goal)
        | helper(card::othercards, Draw::moves', heldcards) = if sum_cards(card::heldcards) > goal 
                                                              then score(card::heldcards, goal) 
                                                              else helper(othercards, moves', card::heldcards)
  in
    helper(cards, moves, [])
  end