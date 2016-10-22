structure IO =
struct
local
    open TextIO.StreamIO
    open Flow
in

fun chars stream =
  fn () =>
     case input1 stream
      of NONE => NIL
       | SOME (x, rest) => CONS (x, chars rest)

end (* local *)
end (* IO *)
