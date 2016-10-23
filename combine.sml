signature COMBINE =
sig
    type 'a f
	    
    val combine : ('a * 'b -> order) *
		  ('a -> 'c) * ('b -> 'c) * ('a * 'b -> 'c)
		  -> 'a f * 'b f -> 'c f
end

signature KEY_COMBINE =
sig
    type 'a f
	    
    val combine : ('a -> 'c) * ('b -> 'c) * ('a * 'b -> 'c)
		  -> 'a f * 'b f -> 'c f
end

structure FlowCombine : COMBINE =
struct
local open Flow
in

type 'a f = 'a flow

fun combine (cmp	: 'a * 'b -> order,
	     left	: 'a -> 'c,
	     right	: 'b -> 'c,
	     center	: 'a * 'b -> 'c)
	    (xs	: 'a flow,
	     ys	: 'b flow)
    : 'c flow =
  let
      fun f (xs, NIL) = map left  xs
	| f (NIL, ys) = map right ys
	| f (xs as CONS (x, xs'), ys as CONS (y, ys')) =
	  case cmp (x, y)
	   of LESS    => CONS (left  x, fn () => f (force xs', ys))
	    | GREATER => CONS (right y, fn () => f (xs, force ys'))
	    | EQUAL => CONS (center (x, y), fn () => f (force xs', force ys'))
  in f (xs, ys)
  end

end (* local *)
end (* FlowCombine *)

structure ListCombine : COMBINE =
struct
local open Flow
in

type 'a f = 'a list

fun combine tuple (xs, ys) =
  list (FlowCombine.combine tuple (flow xs, flow ys))

end (* local *)
end (* ListCombine *)

functor KeyCombine (structure C : COMBINE;
		    structure K : ORD_KEY)
	: KEY_COMBINE =
struct
local
    open KeyValue K
in

type 'a f = (key * 'a) C.f
				       
fun combine (l, r, c) =
  C.combine (keyLift compare, lift l, lift r, lift2 c)

end (* local *)
end (* KeyCombine *)
