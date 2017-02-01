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

functor KeyEmpty (structure E : EMPTY;
		  structure K : KEY)
	: EMPTY =
struct
local
    open KeyValue K
in

type 'a f		= (key * 'a) E.f
				       
val empty		= E.empty

end (* local *)
end (* EmptyEmpty *)

functor LazyEmpty (structure E : EMPTY)
	: EMPTY =
struct
type 'a f = 'a Lazy.thunk E.f
val empty = E.empty
end

functor RefEmpty (structure E : EMPTY)
	: EMPTY =
struct
type 'a f = 'a ref E.f
val empty = E.empty
end
