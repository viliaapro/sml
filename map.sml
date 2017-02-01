signature MAP =
sig
    type 'a f
    val map : ('a -> 'b) -> 'a f -> 'b f
end
    
structure ListMap : MAP =
struct
type 'a f	= 'a list	       
val map		= List.map
end

structure FlowMap : MAP =
struct
type 'a f	= 'a Flow.flow
val map		= Flow.map
end

structure LazyMap : MAP =
struct
local open Lazy
in

type 'a f = 'a thunk

fun map f x = fn () => f (force x)

end (* local *)
end (* LazyMap *)

structure RefMap : MAP =
struct

type 'a f = 'a ref

fun map f = ref o f o (op !)

end (* RefMap *)
    
functor ComposeMap (structure M : MAP;
		    structure N : MAP)
	: MAP =
struct

type 'a f = 'a N.f M.f

fun map f = (M.map o N.map) f

end (* ComposeMap *)

functor KeyMap (type key) : MAP =
struct

type 'a f = key * 'a

fun map f (k, x) = (k, f x)

end (* KeyMap2 *)
