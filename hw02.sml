(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*utils*)
fun exists_in(s: string, string_list:string list) =
    case string_list of
        [] => false
	    | x::local_list => if same_string(s, x) then true else exists_in(s, local_list)
fun remove_from (s: string, string_list:string list) = 
    case string_list of
        [] => []
	    | x::local_list => if same_string(s, x) then remove_from(s, local_list) else x::remove_from(s, local_list)
fun concat_arr (xl, yl) =
    case xl of
        [] => yl
	    | x::xl' => x::concat_arr(xl', yl)

(*1.a*)
fun all_except_option (s:string, string_list:string list) = 
    if not (exists_in(s, string_list)) then NONE else SOME (remove_from(s, string_list))
val tdata_1a_1 = all_except_option("b", ["c", "s", "a", "d", "b", "n"])
(*expected ["c", "s", "a", "d", "n"]*)


(*1.b*)
fun get_substitutions1(ssl: string list list, s:string) = 
    case ssl of
        [] => []
	    | x::ssl' =>
	        let
	            val t = get_substitutions1(ssl', s)
	        in
	            if not(exists_in(s, x)) then t else concat_arr(remove_from(s, x), t)
	        end
(*below took from task*)
val tdata_1b_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
(*expected array of ["Fredrick","Freddie","F"]*)
val tdata_1b_2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
(*expected array of ["Jeffrey","Geoff","Jeffrey"]*)

(*1.c*)
(*this works same as get_substitutions but uses local recursive function*)
fun get_substitutions2 (ssl: string list list, s:string) = 
  let
    fun localf (ssl: string list list, s: string, accu: string list) =
      case ssl of
        [] => accu
	      | x::ssl' =>
	          let
	              val t = if not(exists_in(s, x)) then [] else remove_from(s, x)
	          in
	              localf(ssl', s, concat_arr(accu, t))
	          end
  in
     localf(ssl, s, [])
  end

(*tests for 1.c*)
val tdata_1c_1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
(*expected array of ["Fredrick","Freddie","F"]*)
val tdata_1c_2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
(*expected array of ["Jeffrey","Geoff","Jeffrey"]*)

(*1.d*)
fun similar_names (ssl: string list list, {first=f, middle=m, last=z}) = 
  let
   fun fn_it (sl:string list) = 
     case sl of
        [] => []
	    | x::sl' => {first=x, last=z, middle=m}::fn_it(sl')
   in
    {first=f, last=z, middle=m}::fn_it(get_substitutions2(ssl, f))
   end

val tdata_1d_1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*2a*)
fun card_color (s:suit, r:rank) = 
  case s of 
    Clubs => Black 
    | Diamonds => Red 
    | Hearts => Red 
    | Spades => Black

(*2b*)
fun card_value (s:suit, r:rank) = 
  case r of 
    Ace => 11
    | Num i => i
    | _ => 10

(*2c*)
fun remove_card (cs: card list, c: card, e: exn) = 
    case cs of 
      [] => raise e
      | x::cs' => if x = c then cs' else x::remove_card(cs', c, e)

(*2d*)
fun all_same_color (cs: card list) = 
    case cs of 
      [] => true
      | c::[] => true
      | c1::(c2::cs') => card_color(c1) = card_color(c2) andalso all_same_color(c2::cs')

(*2e*)
fun sum_cards (cs: card list) = 
  let
   fun localf (cs: card list, accu) = 
    case cs of 
      [] => accu
      | c::cs' => localf(cs', card_value(c)+accu)
  in
    localf(cs, 0)
  end

(*2f*)
fun score (cs: card list, goal) = 
   let
      val sum = sum_cards(cs)
      val pre = if sum > goal then 3 * (sum-goal) else goal-sum
   in
      if all_same_color(cs) then pre div 2 else pre
   end

(*2g*)
fun officiate (cs: card list, ds: move list, goal) = 
    let
      fun localf (cs: card list, ds: move list, accu_cs: card list) =
        case (cs, ds) of
	        (_, []) => score(accu_cs, goal)
	      | (_, (Discard c)::ds') => localf(cs, ds', remove_card(accu_cs, c, IllegalMove))
          | ([], Draw::ds') => score(accu_cs, goal)
          | (c::cs', Draw::ds') => 
                if sum_cards(c::accu_cs) > goal 
                then score(c::accu_cs, goal) 
                else localf(cs', ds', c::accu_cs)		   
    in
       localf(cs, ds, [])
    end
