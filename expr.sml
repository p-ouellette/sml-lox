structure Expr:
sig
  datatype t =
    Unary of SourceToken.t * t
  | Binary of t * SourceToken.t * t
  | Grouping of t
  | String of string
  | Number of real
  | Boolean of bool
  | Nil
end =
struct
  datatype t =
    Unary of SourceToken.t * t
  | Binary of t * SourceToken.t * t
  | Grouping of t
  | String of string
  | Number of real
  | Boolean of bool
  | Nil
end
