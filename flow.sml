structure Flow = struct

type 'a thunk = unit -> 'a

fun force x = x ()

datatype 'a flow = NIL | CONS of 'a * 'a flow thunk

fun list flow =
  case flow of
      NIL          => nil
    | CONS (x, xs) => x :: list (force xs)

fun flow list =
  case list of
      []    => NIL
    | x::xs => CONS (x, fn () => flow xs)
			      
fun map f xs =
  case xs of
      NIL => NIL
    | CONS (x,xs') => CONS (f x, fn () => map f (force xs'))

fun app f xs =
  case xs
   of NIL => ()
    | CONS (x, xs') => (f x; app f (force xs'))

fun filter pred xs =
  case xs
   of NIL => NIL
    | CONS (x, xs') =>
      let val tail = fn () => filter pred (force xs')
      in case pred x
	  of true => CONS (x, tail)
	   | false => force tail
      end

fun zip (xs, ys) =
  case (xs, ys)
   of (NIL, _) => NIL
    | (_, NIL) => NIL
    | (CONS (x, xs'), CONS (y, ys')) =>
      CONS ((x, y), fn () => zip (force xs', force ys'))

fun xs @ ys =
  case xs
   of NIL => ys
   | CONS (x, xs') => CONS (x, fn () => force xs' @ ys)


fun foldl f e xs =
  case xs
   of NIL => e
    | CONS (x, xs') => foldl f (f (x, e)) (force xs')
			      
fun foldr f e xs = List.foldr f e (list xs)

fun cartesian (xs, ys) =
  foldl (op @) NIL
	(map (fn x =>
		 map (fn y =>
			 (x, y))
		     ys)
	     xs)
			      
end (* Flow *)
