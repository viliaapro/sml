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
local open Lazy Flow
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
		    structure O : ORD)
	: KEY_COMBINE =
struct
local
    structure M  = KeyMap  (type key = O.t)
    structure M2 = KeyMap2 (type key = O.t)
    open M M2 O
in

type 'a f = (O.t * 'a) C.f
				       
fun combine (l, r, c) =
  let
      fun cmp ((k, _), (k', _)) = compare (k, k')
  in
      C.combine (cmp, map l, map r, map2 c)
  end

end (* local *)
end (* KeyCombine *)

functor MapKeyCombine (structure C : KEY_COMBINE;
		       structure M : MAP;
		       structure M2 : MAP2 where
		       type 'a f = 'a M.f)
	: KEY_COMBINE =
struct
local open M M2
in

type 'a f = 'a M.f C.f

fun combine (l, r, c) = C.combine (map l, map r, map2 c)

end (* local *)
end (* MapKeyCombine *)
