structure Expr:
sig
  datatype t =
    Unary of SourceToken.t * t
  | Binary of t * SourceToken.t * t
  | Grouping of t
  | Nil
  | Boolean of bool
  | Number of real
  | String of string
end =
struct
  datatype t =
    Unary of SourceToken.t * t
  | Binary of t * SourceToken.t * t
  | Grouping of t
  | Nil
  | Boolean of bool
  | Number of real
  | String of string
end
