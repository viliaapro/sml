signature EMPTY =
sig
    type 'a f
    val empty : 'a f
end

structure ListEmpty : EMPTY =
struct
type 'a f	= 'a list
val empty	= nil
end

structure FlowEmpty : EMPTY =
struct
type 'a f	= 'a Flow.flow
val empty	= Flow.NIL
end

functor BoxEmpty (structure F : EMPTY;
		  type 'a box)
	: EMPTY =
struct

type 'a f = 'a box F.f

fun empty p = F.empty

end (* BoxEmpty *)
