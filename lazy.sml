structure Lazy = struct

type 'a thunk = unit -> 'a

fun force x = x ()

fun lift f x = fn () => f (force x)
			  
fun lift2 f (x, y) = fn () => f (force x, force y)
			  
end
