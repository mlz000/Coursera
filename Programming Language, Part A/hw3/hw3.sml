(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals(xs) =
  List.filter(fn s => (Char.isUpper o String.sub)(s, 0)) xs
(* 2 *)
fun longest_string1(xs) =
  List.foldl(fn(x, y) => if String.size x > String.size y then x else y) "" xs
(* 3 *)
fun longest_string2(xs) =
  List.foldl(fn(x, y) => if String.size x >= String.size y then x else y) "" xs
(* 4 *)
fun longest_string_helper f =
  List.foldl(fn(x, y) => if f(String.size(x), String.size(y)) then x else y) ""
val longest_string3 =
  longest_string_helper(fn(x, y) => x > y)
val longest_string4 =
  longest_string_helper(fn(x, y) => x >= y)
(* 5 *)
val longest_capitalized =
  longest_string1 o only_capitals
(* 6 *)
fun rev_string(xs) =
  (String.implode o rev o String.explode) xs
(* 7 *)
fun first_answer f xs =
  case xs of
    [] => raise NoAnswer
  | x::xs' => case f x of
                SOME v => v
              | NONE => first_answer f xs'
(* 8 *)
fun all_answers f xs =
  let fun helper ans lst =
    case lst of
      [] => SOME ans
    | x::xs' => case f x of
                  NONE => NONE
                | SOME v => helper(ans @ v) xs'
  in
    helper [] xs
  end
(* 9a *)
fun count_wildcards(p) =
  g(fn _ => 1) (fn _ => 0) p
(* 9b *)
fun count_wild_and_variable_lengths(p) =
  g(fn _ => 1) String.size p
(* 9c *)
fun count_some_var(str, p) =
  g(fn _ => 0) (fn x => if str = x then 1 else 0) p
(* 10 *)
fun check_pat(p) =
  let
    fun helper1(p1) =
      case p1 of
        Variable x => [x]
      | TupleP ps => List.foldl(fn(p, acc) => helper1(p) @ acc) [] ps
			| ConstructorP(_, p) => helper1(p)
      | _  => []
    fun helper2(lst) =
      case lst of
        [] => false
      | x::xs' => List.exists(fn y => y = x) xs' orelse helper2(xs')
  in
    (not o helper2 o helper1) p
  end
(* 11 *)
fun match(va, pa) =
  case (va, pa) of
    (_, Wildcard) => SOME []
  | (v, Variable s) => SOME [(s, v)]
  | (Unit, UnitP) => SOME []
  | (Const x, ConstP y) => if x = y then SOME [] else NONE
  | (Tuple vs, TupleP ps) => if length(vs) = length(ps) then all_answers match(ListPair.zip(vs, ps)) else NONE
  | (Constructor(s, v), ConstructorP(s',p)) => if s = s' then match(v, p) else NONE
  | _ => NONE
(* 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
