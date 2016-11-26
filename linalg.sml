(* Signatures *)
      
signature MONOID =
sig
    type monoid
	     
    val empty	: monoid
    val append	: monoid * monoid -> monoid
end

signature INNER =
sig
    type s
    type t

    structure Monoid : MONOID

    val inner	: s * t -> Monoid.monoid
end

signature SCALE =
sig
    type t
    type scalar
	     
    val scaleLeft	: scalar * t -> t
    val scaleRight	: t * scalar -> t
end
    
signature TENSOR =
sig
    type s
    type t
    type u
	     
    val tensor	: s * t -> u
end
    
(* Functors *)
				      
functor KeyMonoid (structure E : EMPTY;
		   structure C : KEY_COMBINE where
		   type 'a f = 'a E.f;
		   structure M : MONOID)
	: MONOID =
struct
local
    open Prelude C
in

type monoid	= M.monoid f
val empty	= E.empty
val append	= combine (id, id, M.append)

end (* local *)
end (* KeyMonoid *)

functor KeyInner (structure F : FOLDL;
		  structure C : KEY_COMBINE where
		  type 'a f = 'a F.f;
		  structure I : INNER)
	: INNER =
struct
local
    open Prelude F C I.Monoid
in

type s			= I.s f
type t			= I.t f		    

structure Monoid	= I.Monoid

fun inner (xs, ys) =
  let
      val tuple = (const empty, const empty, I.inner)
  in
      foldl append empty (combine tuple (xs, ys))
  end

end (* local *)
end (* KeyInner *)

functor ScaleLeftInner (structure S : SCALE;
			structure M : MONOID where
			type monoid = S.t)
	: INNER =
struct

type s			= S.scalar
type t			= S.t

structure Monoid	= M
		      
val inner		= S.scaleLeft
		      
end (* ScaleLeftInner *)

functor ScaleRightInner (structure S : SCALE
			 structure M : MONOID where
			 type monoid = S.t)
	: INNER =
struct

type s			= S.t
type t			= S.scalar

structure Monoid	= M
		       
val inner		= S.scaleRight
		      
end (* ScaleRightInner *)
    
functor MapScale (structure M : MAP;
		  structure S : SCALE)
	: SCALE =
struct

type t		= S.t M.f
type scalar	= S.scalar

fun scaleLeft  (a, xs) = M.map (fn x => S.scaleLeft  (a, x)) xs
fun scaleRight (xs, a) = M.map (fn x => S.scaleRight (x, a)) xs

end (* MapScale *)

functor MonoidScale (structure M : MONOID) : SCALE =
struct

type t		= M.monoid
type scalar	= M.monoid

val scaleLeft	= M.append
val scaleRight	= M.append

end (* MonoidScale *)

functor MapTensor (structure M : MAP;
		   structure T : TENSOR)
	: TENSOR =
struct

type s = T.s M.f
type t = T.t
type u = T.u M.f

fun tensor (xs, y) = M.map (fn x => T.tensor (x, y)) xs

end (* MapTensor *)
    
functor ScaleTensor (structure S : SCALE) : TENSOR =
struct

type s		= S.scalar
type t		= S.t
type u		= S.t

val tensor	= S.scaleLeft

end (* ScaleTensor *)
