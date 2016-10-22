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

functor KeyFilter (structure F : FILTER;
		   structure K : KEY)
	: FILTER =
struct
local
    open KeyValue K
in

type 'a f		= (key * 'a) F.f
				       
fun filter p		= F.filter (p o value)

end (* local *)
end (* KeyFilter *)
