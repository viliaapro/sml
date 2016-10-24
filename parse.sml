structure Transduction =
struct
local
    open Flow
    open OptionSearch
in

exception Transduction
			    
fun transduce (f : 'a flow -> ('b * 'a flow) OptionSearch.f) (xs : 'a flow)
    : 'b flow =
  case f xs
   of NONE => raise Transduction
    | SOME (y, xs') => CONS (y, fn () => transduce f xs')

end (* local *)
end (* Transduction *)
	 
functor SearchParse (S : SEARCH) =
struct
local
    structure X = SearchExtra (S)
    open Prelude Flow S X
in

type ('a, 'b) search = 'a -> 'b S.f
type ('a, 'b) parser = ('a flow, 'b * 'a flow) search
				
val get : ('a flow, 'a * 'a flow) search =
 fn NIL          => empty ()
  | CONS (x, xs) => unit (x, force xs)

fun item (f : ('a, 'b) search) : ('a, 'b) parser =
  seq (get, first f)

fun skip (f : ('a, 'b) parser) : ('a flow, 'a flow) search =
  seq (f, unit o snd)
      
fun snarf (f : ('a, 'b) parser) : ('a, 'b list) parser =
  let
      val init = unit o (fn xs => (nil, xs))
      val step = seq (second f,
		      unit o (fn (ys', (y, xs')) => (y::ys', xs')))
  in
      seq (init, cl step)
  end
     
end (* local *)
end (* SearchParse *)
