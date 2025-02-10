structure LoxValue:
sig
  datatype t = Nil | Boolean of bool | Number of real | String of string

  val isTruthy: t -> bool

  val toString: t -> string
end =
struct
  datatype t = Nil | Boolean of bool | Number of real | String of string

  fun isTruthy Nil = false
    | isTruthy (Boolean false) = false
    | isTruthy _ = true

  fun toString Nil = "nil"
    | toString (Boolean b) = Bool.toString b
    | toString (Number n) = Real.toString n
    | toString (String s) = s
end
