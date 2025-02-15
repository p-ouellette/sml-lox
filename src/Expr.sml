structure Expr:
sig
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Variable of SourceToken.t
  | Grouping of t
  | Unary of SourceToken.t * t
  | Binary of t * SourceToken.t * t
  | Assign of SourceToken.t * t
end =
struct
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Variable of SourceToken.t
  | Grouping of t
  | Unary of SourceToken.t * t
  | Binary of t * SourceToken.t * t
  | Assign of SourceToken.t * t
end
