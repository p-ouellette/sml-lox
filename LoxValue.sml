structure LoxValue =
struct
  datatype t = String of string | Number of real | Boolean of bool | Nil

  fun isTruthy (Boolean false) = false
    | isTruthy Nil = false
    | isTruthy _ = true

  fun toString (String s) = s
    | toString (Number n) = Real.toString n
    | toString (Boolean b) = Bool.toString b
    | toString Nil = "nil"
end
