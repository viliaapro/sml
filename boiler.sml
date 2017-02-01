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
