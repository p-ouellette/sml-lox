structure Stmt:
sig
  datatype t = Expression of Expr.t | Print of Expr.t
end =
struct datatype t = Expression of Expr.t | Print of Expr.t end
