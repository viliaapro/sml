structure Prelude =
struct

fun id    x = x
fun const x = fn _ => x

fun curry   f  x  y  = f (x, y);
fun uncurry f (x, y) = f  x  y;
    
fun fst (x, _) = x
fun snd (_, y) = y

end
