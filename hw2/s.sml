(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 1.a *)
fun all_except_option(str, str_list) = 
    case str_list of
    	[] => NONE
      | head::remains =>
        	if same_string(str, head)
        	then SOME remains
        	else case all_except_option(str, remains) of
            	SOME tail => SOME (head::tail)
              | NONE => NONE  

(* Problem 1.b *)
fun get_substitutions1(list, str) = 
    case list of
		[] => []
	  | head::remains =>
	  		case all_except_option(str, head) of
			  	NONE => []
			  | SOME sub_list => sub_list @ get_substitutions1(remains, str)
	
(* Problem 1.c *)
fun get_substitutions2(list, str) =
	let
		fun helper(list, str, acc) = 
			case list of
			[] => acc
			| head::remains =>
				case all_except_option(str, head) of
			  	NONE => helper(remains, str, acc)
			  | SOME sub_list => helper(remains,str,sub_list @ acc)
	in 
		helper(list, str, [])
	end

	 
fun similar_names(list, {first = f, last = l, middle = m}) = 
	let 
		fun helper(subsitutions, acc) =
			case subsitutions of
				[] => acc
			  | head::remains => helper(remains, acc @ [{first = head, last = l, middle = m}])
	in
		helper(get_substitutions2(list, f), [{first = f, last = l, middle = m}])
	end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Solutions for problem 2 *)

(* Problem 2.a *)
fun card_color(card) = 
	case card of
		(Clubs,_) => Black
	  | (Spades,_) => Black
	  | (Diamonds,_) => Red
	  | (Hearts,_) => Red

(* Problem 2.b *)
fun card_value(card) = 
	case card of
		(_,Num n) => n
	  | (_,Ace) => 11
	  | _ => 10

(* Problem 2.c *)
fun remove_card(cards, card, exn) =
	case cards of
		[] => raise exn
	  | head::remains =>
	  		if(head = card)
			then remains
			else head::remove_card(remains, card, exn)

(* Problem 2.d *)
fun all_same_color(cards) = 
	case cards of
		first::second::remains => (card_color(first) = card_color(second)) andalso all_same_color(second::remains)
	  | _::[] => true
	  | [] => true

(* Problem 2.e *)
fun sum_cards(cards) = 
	let fun sum(cards, acc) = 
		case cards of
			[] => acc
	  	  | head::remains =>  sum(remains, acc + card_value(head))
	in 
		sum(cards, 0)
	end

(* Problem 2.f *)
fun score(cards, goal) = 
	let 
		val sum = sum_cards(cards)
		val preliminary_score = if(sum > goal)
								then 3 * (sum - goal)
								else goal - sum
	in
		if(all_same_color(cards))
		then preliminary_score div 2
		else preliminary_score
	end

(* Problem 2.g *)
fun officiate(cards, moves, goal) =
	let fun helper(held, cards, moves) =  
		case moves of
		 	[] => score(held, goal)
		  | head::remain_moves => 
		  		case head of
					Discard card => helper(remove_card(held, card, IllegalMove), cards, remain_moves)
				  | Draw => case cards of
				  				[] => score(held, goal)
							  | head::remain_cards => if(sum_cards(head::held) > goal)
							  					 then score(head::held, goal)
												 else helper(head::held, remain_cards, remain_moves)
	in
		helper([], cards, moves)
	end

	