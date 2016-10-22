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

functor KeyMap (structure M : MAP;
		structure K : KEY)
	: MAP =
struct
local
    open KeyValue K
in

type 'a f		= (key * 'a) M.f
				       
fun map f		= M.map (lift f)

end (* local *)
end (* KeyMap *)
