functor Tree (type 'a f) =
struct
datatype 'a tree = TREE of 'a * 'a tree f
end

structure ParseTree =
struct
local
    structure T = Tree (type 'a f = 'a list)
    structure S = OptionSearch
    structure X = SearchExtra (S)
    structure P = SearchParse (S)
    open Prelude Flow T S X P
in

fun tree (p : 'a * 'a -> bool) (parent : 'a) : ('a, 'a tree) parser =
  fn NIL => empty ()
   | CONS (x, xs') =>
     if p (parent, x)
     then seq (snarf (tree p x),
	       first (unit o (curry TREE x) o rev))
	      (force xs')
     else empty ()
	 
end (* local *)
end (* ParseTree *)
    
functor Unfold (structure M : MAP;
		structure C : KEY_COMBINE where
		type 'a f = 'a M.f;
		structure F : FILTER where
		type 'a f = 'a M.f) =
struct
local
    structure T = Tree (type 'a f = 'a M.f)
    open Prelude KeyValue M C F T
in

fun unfold (graph : 'a f f) (root : 'a f) : 'a tree f =
  let
      fun func (xs, y) = TREE (y, unfold graph xs)
      val tuple = (const NONE, const NONE, SOME o func)
  in
      (map valOf o filter isSome) (combine tuple (graph, root))
  end
    
end (* local *)
end (* Unfold *)
