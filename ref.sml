structure Ref = struct

fun lift f = ref o f o (op !)
			  
fun lift2 f (ref x, ref y) = ref (f (x, y))
			  
end
