structure MutList = struct

datatype 'a mutlist = NIL | CONS of 'a * 'a mutlist ref

fun list flow =
  case flow of
      NIL          => nil
    | CONS (x, ref xs) => x :: list xs

fun mutlist list =
  case list of
      []    => NIL
    | x::xs => CONS (x, ref (mutlist xs))
			      
fun map f xs =
  case xs of
      NIL => NIL
    | CONS (x, ref xs') => CONS (f x, ref (map f xs'))

fun app f xs =
  case xs
   of NIL => ()
    | CONS (x, xs') => (f x; app f (!xs'))

fun filter pred xs =
  case xs
   of NIL => NIL
    | CONS (x, ref xs') =>
      let val tail = filter pred xs'
      in case pred x
	  of true  => CONS (x, ref tail)
	   | false => tail
      end

fun zip (xs, ys) =
  case (xs, ys)
   of (NIL, _) => NIL
    | (_, NIL) => NIL
    | (CONS (x, ref xs'), CONS (y, ref ys')) =>
      CONS ((x, y), ref (zip (xs', ys')))

fun xs @ ys =
  case xs
   of NIL => ys
   | CONS (x, ref xs') => CONS (x, ref (xs' @ ys))

fun foldl f e xs =
  case xs
   of NIL => e
    | CONS (x, ref xs') => foldl f (f (x, e)) xs'
			      
fun foldr f e xs = List.foldr f e (list xs)

fun cartesian (xs, ys) =
  foldl (op @) NIL
	(map (fn x =>
		 map (fn y =>
			 (x, y))
		     ys)
	     xs)

end (* MutList *)
