structure Value:
sig
  datatype t = Nil | Boolean of bool | Number of real

  val print: t -> unit
end =
struct
  datatype t = Nil | Boolean of bool | Number of real

  fun print Nil = TextIO.print "nil"
    | print (Boolean b) =
        TextIO.print (Bool.toString b)
    | print (Number n) =
        TextIO.print (Real.toString n)
end
