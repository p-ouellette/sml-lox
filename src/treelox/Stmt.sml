structure Stmt =
struct
  datatype t =
    Class of
      { name: SourceToken.t
      , superclass: SourceToken.t option
      , methods: function list
      }
  | Function of function
  | Var of {name: SourceToken.t, initializer: Expr.t}
  | Expression of Expr.t
  | If of {condition: Expr.t, thenBranch: t, elseBranch: t option}
  | Print of Expr.t
  | Return of SourceToken.t * Expr.t
  | While of {condition: Expr.t, body: t}
  | Block of t list
  withtype function =
    {name: SourceToken.t, params: SourceToken.t list, body: t list}
end
