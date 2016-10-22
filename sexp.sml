structure Sexp =
struct
local
    structure S = OptionSearch
    structure X = SearchExtra (S)
    structure P = SearchParse (S)
    open Prelude Flow S X P Transduction
		 
    fun f * g = seq (f, g)
    fun f + g = alt (f, g)

    fun cond p f x = if p x then unit (f x) else empty ()
in

datatype token = S of string | LPAREN | RPAREN
datatype expr  = A of string | L of expr list
		     
val lparen	= cond (curry (op =) #"(") (const LPAREN)
val rparen	= cond (curry (op =) #")") (const RPAREN)

val alphanum	= cond Char.isAlphaNum id
val space	= cond Char.isSpace    id
			 
val word	= snarf (item alphanum) * first (unit o S o String.implode)

val token	= cl ((skip o item) space) * (item (lparen + rparen) + word)

fun atom (xs : token flow) : (expr * token flow) S.f  =
  item (fn S s => unit (A s) | _ => empty ()) xs

and list (xs : token flow) : (expr * token flow) S.f  =
    let
	fun drop (x : token)
	  = (skip o item) (cond (curry (op =) x) (const ()))
    in
	(drop LPAREN *
	 snarf expr * first (unit o L o rev) *
	 second (drop RPAREN)) xs
    end
			   
and expr (xs : token flow) : (expr * token flow) S.f  =
    (atom + list) xs

val tokens = transduce token
val exprs  = transduce expr
		  
end (* local *)
end (* Sexp *)
