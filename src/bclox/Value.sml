structure Value:
sig
  datatype t = Nil | Boolean of bool | Number of real

  val isFalsy: t -> bool
  val print: t -> unit
end =
struct
  datatype t = Nil | Boolean of bool | Number of real

  fun isFalsy Nil = true
    | isFalsy (Boolean b) = not b
    | isFalsy _ = false

  fun print Nil = TextIO.print "nil"
    | print (Boolean b) =
        TextIO.print (Bool.toString b)
    | print (Number n) =
        TextIO.print (Real.toString n)
end
