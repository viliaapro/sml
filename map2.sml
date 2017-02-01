signature MAP2 =
sig
    type 'a f
    val map2 : ('a * 'b -> 'c) -> 'a f * 'b f -> 'c f
end
    
structure LazyMap2 : MAP2 =
struct
local open Lazy
in

type 'a f = 'a thunk

fun map2 f (x, y) = fn () => f (force x, force y)

end (* local *)
end (* LazyMap2 *)

structure RefMap2 : MAP2 =
struct

type 'a f = 'a ref

fun map2 f (ref x, ref y) = ref (f (x, y))

end (* RefMap2 *)

functor KeyMap2 (type key) : MAP2 =
struct

type 'a f = key * 'a

fun map2 f ((k, x), (_, y)) = (k, f (x, y))

end (* KeyMap2 *)
