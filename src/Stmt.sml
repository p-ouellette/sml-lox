structure Stmt:
sig
  datatype t =
  (* declarations *)
    Function of {name: SourceToken.t, params: SourceToken.t list, body: t list}
  | Var of {name: SourceToken.t, initializer: Expr.t}
  (* statements *)
  | Expression of Expr.t
  | If of {condition: Expr.t, thenBranch: t, elseBranch: t option}
  | Print of Expr.t
  | While of {condition: Expr.t, body: t}
  | Block of t list
end =
struct
  datatype t =
    Function of {name: SourceToken.t, params: SourceToken.t list, body: t list}
  | Var of {name: SourceToken.t, initializer: Expr.t}
  | Expression of Expr.t
  | If of {condition: Expr.t, thenBranch: t, elseBranch: t option}
  | Print of Expr.t
  | While of {condition: Expr.t, body: t}
  | Block of t list
end
