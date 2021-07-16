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

(* Solution 1 *)
fun start_with_captial str = Char.isUpper(String.sub(str,0))
val only_capitals = List.filter start_with_captial

(* Solution 2 *)
val longest_string1 = foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) ""

(* Solution 3 *)
val longest_string2 = foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) ""

(* Solution 4 *)
fun longest_string_helper f = List.foldl(fn(x,y) => if f(String.size x,String.size y) then x else y) ""
val longest_string3 = longest_string_helper (fn(x, y) => x > y)
val longest_string4 = longest_string_helper (fn(x, y) => x >= y)

(* Solution 5 *)
fun longest_capitalized strl = (longest_string1 o only_capitals) strl

(* Solution 6 *)
val rev_string = String.implode o List.rev o String.explode

(* Solution 7 *)
fun first_answer f lst = 
    case lst of
        [] => raise NoAnswer
      | head::tail => case f head of
                        NONE => first_answer f tail
                      | SOME(res) => res

(* Solution 8 *)
fun all_answers f lst = 
    let
        fun helper (lst',acc) = 
            case lst' of
                [] => SOME acc
              | head::tail => case (f head) of
                        NONE => NONE
                      | SOME res => helper (tail, res @ acc)
    in
      helper (lst, [])
    end

(* Solution 9 *)
val count_wildcards = g (fn () => 1) (fn str => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size(s))
fun count_some_var (s, p) = g (fn () => 0) (fn str => if str = s then 1 else 0) p

(* Solution 10 *)
fun check_pat p =
    let
        fun collect p' =
            case p' of
                Variable x        => [x]
              | TupleP ps         => List.foldl (fn (p,i) => (collect p) @ i) [] ps
              | ConstructorP(_,p) => collect p
              | _                 => []
        fun exist_dup strl = 
            case strl of 
                [] => false
            |  head::tail => List.exists (fn str => head = str) tail orelse exist_dup tail
    in
        not ((exist_dup o collect) p)
    end

(* Solution 11 *)
fun match (v, p) = 
    case (v, p) of
        (_, Wildcard) => SOME []
      | (v, Variable s)  => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => 
            if List.length vs = List.length ps 
            then all_answers match (ListPair.zip(vs, ps))
            else NONE
      | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match(v, p) else NONE
      | (_, _) => NONE

(* Solution 12 *)
fun first_match v ps = 
    SOME(first_answer (fn p => match(v, p)) ps) 
    handle NoAnswer => NONE