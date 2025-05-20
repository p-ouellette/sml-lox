structure Class:
sig
  val findMethod: Value.class * string -> Value.function option
  val arity: Value.class -> int
end =
struct
  fun findMethod (class: Value.class, name) =
    StringMap.find (#methods (!class), name)

  fun arity class =
    case findMethod (class, "init") of
      SOME init => Function.arity init
    | NONE => 0
end
