# sml
Some SML code

In `prelude.sml`, there are some basic functions like `id`.

In `flow.sml`, lazy lists; in `mutlist.sml`, mutable lists.

In files carrying names of list functions, there are signatures and functors relating to such. Boilerplate code is in `boiler.sml`.

In `combine.sml`, something akin to relational join.

In `linalg.sml`, rudiments of linear algebra.

In `search.sml`, machinery for search with Kleisli arrows with the usual Kleene algebra operations. This is used by the parser. Simple parsers for s-expressions and indented trees are presented in `sexp.sml` and `tree.sml`.
