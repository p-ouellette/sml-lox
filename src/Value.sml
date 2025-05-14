structure Value:
sig
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of {repr: string, arity: int, call: t list -> t}
  | Function of {declaration: Stmt.function, closure: t Environment.t}
  | Class of class
  | Instance of {class: class, fields: t StringMap.map} ref
  withtype class =
    { name: string
    , methods: {declaration: Stmt.function, closure: t Environment.t} StringMap.map
    }

  type function = {declaration: Stmt.function, closure: t Environment.t}
  type instance = {class: class, fields: t StringMap.map} ref

  val isTruthy: t -> bool
  val isEqual: t * t -> bool
end =
struct
  structure Env = Environment

  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of {repr: string, arity: int, call: t list -> t}
  | Function of {declaration: Stmt.function, closure: t Env.t}
  | Class of class
  | Instance of {class: class, fields: t StringMap.map} ref
  withtype class =
    { name: string
    , methods: {declaration: Stmt.function, closure: t Env.t} StringMap.map
    }

  type function = {declaration: Stmt.function, closure: t Env.t}
  type instance = {class: class, fields: t StringMap.map} ref

  fun isTruthy Nil = false
    | isTruthy (Boolean false) = false
    | isTruthy _ = true

  fun isEqual (Nil, Nil) = true
    | isEqual (Boolean a, Boolean b) = a = b
    | isEqual (Number a, Number b) = Real.== (a, b)
    | isEqual (String a, String b) = a = b
    | isEqual (Instance a, Instance b) = a = b
    | isEqual _ = false
end
