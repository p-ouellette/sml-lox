structure Value:
sig
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of {arity: int, call: t list -> t} ref
  | Function of function
  | Class of {name: string, methods: function StringMap.map} ref
  | Instance of
      { class: {name: string, methods: function StringMap.map} ref
      , fields: t StringMap.map
      } ref
  withtype function =
    {declaration: Stmt.function, closure: t Environment.t, isInitializer: bool} ref

  type class = {name: string, methods: function StringMap.map} ref
  type instance = {class: class, fields: t StringMap.map} ref

  val isTruthy: t -> bool
  val isEqual: t * t -> bool
end =
struct
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of {arity: int, call: t list -> t} ref
  | Function of function
  | Class of {name: string, methods: function StringMap.map} ref
  | Instance of
      { class: {name: string, methods: function StringMap.map} ref
      , fields: t StringMap.map
      } ref
  withtype function =
    {declaration: Stmt.function, closure: t Environment.t, isInitializer: bool} ref

  type class = {name: string, methods: function StringMap.map} ref
  type instance = {class: class, fields: t StringMap.map} ref

  fun isTruthy Nil = false
    | isTruthy (Boolean false) = false
    | isTruthy _ = true

  fun isEqual (Nil, Nil) = true
    | isEqual (Boolean a, Boolean b) = a = b
    | isEqual (Number a, Number b) = Real.== (a, b)
    | isEqual (String a, String b) = a = b
    | isEqual (Builtin a, Builtin b) = a = b
    | isEqual (Function a, Function b) = a = b
    | isEqual (Class a, Class b) = a = b
    | isEqual (Instance a, Instance b) = a = b
    | isEqual _ = false
end
