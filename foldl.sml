signature FOLDL =
sig
    type 'a f
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a f -> 'b
end

structure ListFoldl : FOLDL =
struct
type 'a f		= 'a list
val foldl		= List.foldl
end

structure FlowFoldl : FOLDL =
struct
type 'a f		= 'a Flow.flow
val foldl		= Flow.foldl
end

functor KeyFoldl (structure F : FOLDL;
		  structure K : KEY)
	: FOLDL =
struct
local open KeyValue K
in

type 'a f		= (key * 'a) F.f
				       
fun foldl f		= F.foldl (fn (x, y) => f (value x, y))

end (* local *)
end (* KeyFoldl *)

functor LazyFoldl (structure F : FOLDL)
	: FOLDL =
struct
local
    open Lazy
in

type 'a f = 'a thunk F.f

fun foldl f = F.foldl (fn (x, y) => f (force x, y))

end (* local *)
end (* LazyFoldl *)

functor RefFoldl (structure F : FOLDL)
	: FOLDL =
struct

type 'a f = 'a ref F.f

fun foldl f = F.foldl (fn (x, y) => f (!x, y))

end (* RefFoldl *)
