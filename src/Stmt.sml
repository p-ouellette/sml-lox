structure Stmt:
sig
  datatype t =
  (* declarations *)
    Var of SourceToken.t * Expr.t
  (* statements *)
  | Expression of Expr.t
  | Print of Expr.t
  | Block of t list
end =
struct
  datatype t =
    Var of SourceToken.t * Expr.t
  | Expression of Expr.t
  | Print of Expr.t
  | Block of t list
end
