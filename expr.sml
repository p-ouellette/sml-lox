structure Expr:
sig
  datatype t =
    Unary of Token.t * t
  | Binary of t * Token.t * t
  | Grouping of t
  | String of string
  | Number of real
  | Boolean of bool
  | Nil
end =
struct
  datatype t =
    Unary of Token.t * t
  | Binary of t * Token.t * t
  | Grouping of t
  | String of string
  | Number of real
  | Boolean of bool
  | Nil
end
