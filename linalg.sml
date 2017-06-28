(* Signatures *)
      
signature MAGMA =
sig
    type m
	     
    val append	: m * m -> m
end

signature INNER =
sig
    type s
    type t

    structure Magma : MAGMA

    val empty	: Magma.m
    val inner	: s * t -> Magma.m
end

signature SCALE_LEFT =
sig
    type t
    type scalar
	     
    val scale	: scalar * t -> t
end

signature SCALE_RIGHT =
sig
    type t
    type scalar
	     
    val scale	: t * scalar -> t
end
    
signature TENSOR =
sig
    type s
    type t
    type u
	     
    val tensor	: s * t -> u
end
    
(* Functors *)
				      
functor KeyMagma (structure C : KEY_COMBINE;
		  structure M : MAGMA)
	: MAGMA =
struct
local
    open Prelude C
in

type m		= M.m f
val append	= combine (id, id, M.append)

end (* local *)
end (* KeyMagma *)

functor KeyInner (structure F : FOLDL;
		  structure C : KEY_COMBINE where
		  type 'a f = 'a F.f;
		  structure I : INNER)
	: INNER =
struct
local
    open Prelude F C I.Magma
in

type s	= I.s f
type t	= I.t f		    

structure Magma	= I.Magma

val empty = I.empty
		      
fun inner (xs, ys) =
  let
      val tuple = (const empty, const empty, I.inner)
  in
      foldl append empty (combine tuple (xs, ys))
  end

end (* local *)
end (* KeyInner *)

functor ScaleInnerLeft (structure S : SCALE_LEFT;
			structure M : MAGMA where
			type m = S.t;
			val empty : M.m)
	: INNER =
struct

type s			= S.scalar
type t			= S.t

structure Magma	= M

val empty		= empty
val inner		= S.scale
		      
end (* ScaleInnerLeft *)

functor ScaleInnerRight (structure S : SCALE_RIGHT
			 structure M : MAGMA where
			 type m = S.t;
			 val empty : M.m)
	: INNER =
struct

type s			= S.t
type t			= S.scalar

structure Magma	= M
		       
val empty		= empty
val inner		= S.scale
		      
end (* ScaleInnerRight *)
    
functor MapScaleLeft (structure M : MAP;
		      structure S : SCALE_LEFT)
	: SCALE_LEFT =
struct

type t		= S.t M.f
type scalar	= S.scalar

fun scale (a, xs) = M.map (fn x => S.scale (a, x)) xs

end (* MapScaleLeft *)

functor MapScaleRight (structure M : MAP;
		       structure S : SCALE_RIGHT)
	: SCALE_RIGHT =
struct

type t		= S.t M.f
type scalar	= S.scalar

fun scale (xs, a) = M.map (fn x => S.scale (x, a)) xs

end (* MapScaleRight *)
    
functor MagmaScaleLeft (structure M : MAGMA) : SCALE_LEFT =
struct

type t		= M.m
type scalar	= M.m

val scale	= M.append

end (* MagmaScaleLeft *)

functor MagmaScaleRight (structure M : MAGMA) : SCALE_RIGHT =
struct

type t		= M.m
type scalar	= M.m

val scale	= M.append

end (* MagmaScaleRight *)
    
functor MapTensor (structure M : MAP;
		   structure T : TENSOR)
	: TENSOR =
struct

type s = T.s M.f
type t = T.t
type u = T.u M.f

fun tensor (xs, y) = M.map (fn x => T.tensor (x, y)) xs

end (* MapTensor *)
    
functor ScaleTensorLeft (structure S : SCALE_LEFT) : TENSOR =
struct

type s		= S.scalar
type t		= S.t
type u		= S.t

val tensor	= S.scale

end (* ScaleTensor *)

functor ScaleTensorRight (structure S : SCALE_RIGHT) : TENSOR =
struct

type s		= S.t
type t		= S.scalar
type u		= S.t

val tensor	= S.scale

end (* ScaleTensor *)
