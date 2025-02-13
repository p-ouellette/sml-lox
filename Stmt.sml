structure Stmt:
sig
  datatype t =
    Expression of Expr.t
  | Print of Expr.t
  | Var of SourceToken.t * Expr.t
end =
struct
  datatype t =
    Expression of Expr.t
  | Print of Expr.t
  | Var of SourceToken.t * Expr.t
end
