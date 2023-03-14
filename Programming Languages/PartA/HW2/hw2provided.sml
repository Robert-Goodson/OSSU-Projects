(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, xs)=
    case xs of
	[] => NONE
      | x::xs' => if same_string(x,s)  then SOME xs'
		  else case all_except_option(s, xs') of
			   NONE =>NONE
			| SOME y => SOME(x::y)

fun get_substitutions1 (subs, name)=
    case subs of
	[]=>[]
      | x::xs  => case all_except_option(name, x)of
		     NONE =>get_substitutions1(xs,name)
		   | SOME y => y@get_substitutions1(xs,name)

fun get_substitutions2 (subs, name)=
    let
	fun helper(acc, lst) =
	    case lst of
		[]=> acc
	      | x::xs => helper( (case all_except_option(name, x) of
					  NONE => acc
					| SOME y => acc @ y), xs)
				    				    
   in
	helper([],subs)
    end
	
fun similar_names (names,name)=
    let
	val {first = f, middle = m, last = l}= name
	fun make_names (xs) =
	    case xs of
		[]=>[]
	      | x::xs' =>{first=x,middle=m,last = l} :: make_names(xs')
								  
    in
	name::make_names(get_substitutions2(names,f))
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
fun card_color (card) =
    case card of
	(Diamonds,_)  => Red
      |(Hearts,_) => Red
      |( Clubs,_) => Black
      | (Spades,_) =>Black
			 

fun card_value ( _,Num x) = x
  | card_value (_,Ace)=11
  | card_value (_,King) = 10
  | card_value (_,Queen)  = 10
  | card_value (_,Jack) = 10 

fun remove_card (cs, c, e) =
    case cs of
	[]=> raise e
      | x::cs' => if x = c  then cs' else x::remove_card(cs',c,e)
						   
fun all_same_color (cards)=
    case cards of
	card1::card2::crds => card_color(card1)=card_color(card2) andalso
			       all_same_color(card2::crds)
      | _ => true

fun sum_cards cards =
    let
	fun helper (cards, acc)=
	    case cards of
		[]=>acc
	      | x::xs => helper(xs,card_value x+acc)
    in
	helper(cards, 0)
    end
	
fun score (cards, goal)=
    let
	val sum = sum_cards cards
			  
    in
	(if sum >= goal then 3 * (sum-goal) else goal - sum)
	div (if all_same_color cards then 2 else 1)
    end

fun officiate (cards, moves, goal) =
    let
	fun eval (hand, cards_left, moves_left)=
	        case moves_left of
		    [] => score (hand, goal)
		  | (Discard x)::xs => eval(remove_card(hand ,x ,IllegalMove),cards_left,xs)
		  | Draw::xs =>case cards_left of
				   [] => score(hand,goal)
				 | y::ys =>if  sum_cards(y::hand) > goal
					  then score(y::hand,goal)
					  else  eval(y::hand,ys,xs)
    in
	eval([],cards,moves)
    end
