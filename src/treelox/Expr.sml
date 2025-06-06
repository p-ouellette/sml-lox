structure Expr =
struct
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Variable of SourceToken.t
  | This of SourceToken.t
  | Grouping of t
  | Super of {keyword: SourceToken.t, method: SourceToken.t}
  | Call of {callee: t, paren: SourceToken.t, arguments: t list}
  | Get of t * SourceToken.t
  | Unary of SourceToken.t * t
  | Binary of t * SourceToken.t * t
  | Logical of t * SourceToken.t * t
  | Set of t * SourceToken.t * t
  | Assign of SourceToken.t * t
end
