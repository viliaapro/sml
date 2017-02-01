signature FILTER =
sig
    type 'a f
    val filter : ('a -> bool) -> 'a f -> 'a f
end

structure ListFilter : FILTER =
struct
type 'a f		= 'a list
val filter		= List.filter
end

structure FlowFilter : FILTER =
struct
type 'a f		= 'a Flow.flow
val filter		= Flow.filter
end

functor BoxFilter (structure F : FILTER;
		     type 'a box;
		     val unbox : 'a box -> 'a)
	: FILTER =
struct

type 'a f = 'a box F.f

fun filter p = F.filter (p o unbox)

end (* BoxFilter *)
