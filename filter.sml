signature FILTER =
sig
    type 'a f
    val filter : ('a -> bool) -> 'a f -> 'a f
end

functor BoxFilter (structure F : FILTER;
		   type 'a box;
		   val unbox : 'a box -> 'a)
	: FILTER =
struct

type 'a f = 'a box F.f

fun filter p = F.filter (p o unbox)

end (* BoxFilter *)
