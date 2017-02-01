signature ORD =
sig
    type t	     
    val compare : t * t -> order
end

structure Prelude =
struct

fun id    x = x
fun const x = fn _ => x

fun curry   f  x  y  = f (x, y);
fun uncurry f (x, y) = f  x  y;
    
fun fst (x, _) = x
fun snd (_, y) = y

end (* Prelude *)

structure Lazy = struct

type 'a thunk = unit -> 'a

fun force x = x ()
			  
end (* Lazy *)
