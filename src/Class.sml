structure Class:
sig
  val new: {name: string, methods: Value.function StringMap.map} -> Value.class
  val name: Value.class -> string
  val findMethod: Value.class * string -> Value.function option
  val arity: Value.class -> int
  val toString: Value.class -> string
end =
struct
  fun new x = ref x

  fun name (class: Value.class) =
    #name (!class)

  fun findMethod (class: Value.class, name) =
    StringMap.find (#methods (!class), name)

  fun arity class =
    case findMethod (class, "init") of
      SOME init => Function.arity init
    | NONE => 0

  fun toString class = name class
end
