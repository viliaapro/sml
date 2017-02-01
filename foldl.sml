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

functor BoxFoldl (structure F : FOLDL;
		  type 'a box;
		  val unbox : 'a box -> 'a)
	: FOLDL =
struct

type 'a f = 'a box F.f
	       
fun foldl f = F.foldl (fn (x, y) => f (unbox x, y))

end (* BoxFoldl *)
