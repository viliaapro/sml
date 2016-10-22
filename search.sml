signature SEARCH =
sig
    type 'a f

    val unit		: 'a -> 'a f
    val empty		: 'a -> 'b f

    val seq		: ('a -> 'b f) * ('b -> 'c f) -> ('a -> 'c f)
    val alt		: ('a -> 'b f) * ('a -> 'b f) -> ('a -> 'b f)

    val par 		: ('a -> 'b f) * ('c -> 'd f) ->
			  'a * 'c -> ('b * 'd) f
end

signature SEARCH_EXTRA =
sig    
    type 'a f
    
    val cl		: ('a -> 'a f) -> 'a -> 'a f
					      
    val first		: ('a -> 'b f) -> 'a * 'c -> ('b * 'c) f
    val second		: ('a -> 'b f) -> 'c * 'a -> ('c * 'b) f
end
    
structure OptionSearch : SEARCH =
struct
local
    open Prelude
in

type 'a f = 'a option
	       
fun unit  x = SOME x
fun empty x = NONE
		
fun seq (f, g) x =
  case f x
   of NONE   => NONE
    | SOME y => g y
		  
fun alt (f, g) x =
  case f x
   of NONE   => g x
    | ys     => ys

fun par (f, g) (x, y) =
  case (f x, g y)
   of (NONE, _) => NONE
    | (_, NONE) => NONE
    | (SOME x', SOME y') => SOME (x', y')

end (* local *)
end (* OptionSearch *)

structure FlowSearch : SEARCH =
struct
local
    open Prelude Flow
in

type 'a f = 'a flow

fun unit  x			= flow [x]
fun empty x			= NIL

fun seq (f, g) x		= foldl (op @) NIL (map g (f x))
fun alt (f, g) x		= f x @ g x
			  
fun par (f, g) (x, y)		= cartesian (f x, g y)
					
end (* local *)
end (* FlowSearch *)

functor SearchExtra (S : SEARCH) : SEARCH_EXTRA =
struct
local
    open S
in

type 'a f = 'a S.f

fun cl f x = alt (seq (f, cl f), unit) x (* bug in compiler w/o x *)

fun first  f = par (f, unit)
fun second f = par (unit, f)

end (* local *)
end (* SearchExtra*)
