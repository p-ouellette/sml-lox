structure Value:
sig
  datatype t = Nil | Boolean of bool | Number of real | String of string

  val isEqual: t * t -> bool
  val isFalsy: t -> bool
  val print: t -> unit
end =
struct
  datatype t = Nil | Boolean of bool | Number of real | String of string

  fun isEqual (Nil, Nil) = true
    | isEqual (Boolean a, Boolean b) = a = b
    | isEqual (Number a, Number b) = Real.== (a, b)
    | isEqual (String a, String b) = a = b
    | isEqual _ = false

  fun isFalsy Nil = true
    | isFalsy (Boolean b) = not b
    | isFalsy _ = false

  fun print Nil = TextIO.print "nil"
    | print (Boolean b) =
        TextIO.print (Bool.toString b)
    | print (Number n) =
        TextIO.print (Real.toString n)
    | print (String s) = TextIO.print s
end
