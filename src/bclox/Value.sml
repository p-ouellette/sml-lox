structure Value:
sig
  type t
  val toString: t -> string
end =
struct
  type t = real

  fun toString value = Real.toString value
end
