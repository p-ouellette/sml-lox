structure Builtin:
sig
  val new: {arity: int, call: Value.t list -> Value.t} -> Value.builtin
  val arity: Value.builtin -> int
  val call: Value.builtin -> Value.t list -> Value.t
  val toString: Value.builtin -> string
end =
struct
  fun new x = ref x

  fun arity (b: Value.builtin) =
    #arity (!b)

  fun call (b: Value.builtin) =
    #call (!b)

  fun toString _ = "<native fn>"
end
