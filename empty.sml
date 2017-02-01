signature EMPTY =
sig
    type 'a f
    val empty : 'a f
end

functor BoxEmpty (structure E : EMPTY;
		  type 'a box)
	: EMPTY =
struct

type 'a f = 'a box E.f

val empty = E.empty

end (* BoxEmpty *)
