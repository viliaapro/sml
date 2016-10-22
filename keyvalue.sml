signature KEY =
sig
    type key
end

signature ORD_KEY =
sig
    type key
	     
    val compare : (key * key) -> order
end


structure KeyValue =
struct

fun key   (k, x) = k
fun value (k, x) = x
    
fun lift  f (k, x)		= (k, f x)
fun lift2 f ((k, x), (_, y))	= (k, f (x, y))

fun keyLift f (p,q) = f (key p, key q)
				      
end (* KeyValue *)
