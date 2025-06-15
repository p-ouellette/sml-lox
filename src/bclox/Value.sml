structure Value:
sig
  type t
  val print: t -> unit
end =
struct
  type t = real

  fun print value =
    TextIO.print (Real.toString value)
end
