(* This structure only reports errors; it doesn't need to resolve variables like
 * the book does because we use a persistent data structure for environments.
 *)
structure Resolver:
sig
  val resolve: Stmt.t list -> unit
end =
struct
  structure M = StringRedBlackMap

  structure FunctionType = struct datatype t = NONE | FUNCTION end

  fun beginScope scopes = M.empty :: scopes
  fun endScope scopes = tl scopes

  fun declare ([], _) = []
    | declare (s :: ss, name: SourceToken.t) =
        ( if M.inDomain (s, #lexeme name) then
            Error.errorAt
              (name, "Already a variable with this name in this scope.")
          else
            ()
        ; M.insert (s, #lexeme name, false) :: ss
        )

  fun define ([], _) = []
    | define (s :: ss, name: SourceToken.t) =
        M.insert (s, #lexeme name, true) :: ss

  fun resolveStmt _ (Stmt.Class {name, methods = _}, ss) =
        define (declare (ss, name), name)
    | resolveStmt _ (Stmt.Function {name, params, body}, ss) =
        let val ss = define (declare (ss, name), name)
        in resolveFunction (params, body, ss, FunctionType.FUNCTION)
        end
    | resolveStmt _ (Stmt.Var {name, initializer}, ss) =
        let val ss = resolveExpr (initializer, declare (ss, name))
        in define (ss, name)
        end
    | resolveStmt _ (Stmt.Expression expr, ss) = resolveExpr (expr, ss)
    | resolveStmt func (Stmt.If {condition, thenBranch, elseBranch}, ss) =
        let
          val ss = resolveExpr (condition, ss)
          val ss = resolveStmt func (thenBranch, ss)
        in
          case elseBranch of
            SOME stmt => resolveStmt func (stmt, ss)
          | NONE => ss
        end
    | resolveStmt _ (Stmt.Print expr, ss) = resolveExpr (expr, ss)
    | resolveStmt func (Stmt.Return (token, expr), ss) =
        ( if func = FunctionType.NONE then
            Error.errorAt (token, "Can't return from top-level code.")
          else
            ()
        ; resolveExpr (expr, ss)
        )
    | resolveStmt func (Stmt.While {condition, body}, ss) =
        resolveExpr (condition, resolveStmt func (body, ss))
    | resolveStmt func (Stmt.Block stmts, ss) =
        endScope (resolveStmts (stmts, beginScope ss, func))

  and resolveFunction (params, body, ss, func) =
    let
      val ss = beginScope ss
      val ss = foldl (fn (p, ss) => define (declare (ss, p), p)) ss params
      val ss = resolveStmts (body, ss, func)
    in
      endScope ss
    end

  and resolveExpr (Expr.Nil, ss) = ss
    | resolveExpr (Expr.Boolean _, ss) = ss
    | resolveExpr (Expr.Number _, ss) = ss
    | resolveExpr (Expr.String _, ss) = ss
    | resolveExpr (Expr.Variable name, ss as s :: _) =
        ( if M.find (s, #lexeme name) = SOME false then
            Error.errorAt
              (name, "Can't read local variable in its own initializer.")
          else
            ()
        ; ss
        )
    | resolveExpr (Expr.Variable _, ss) = ss
    | resolveExpr (Expr.Grouping expr, ss) = resolveExpr (expr, ss)
    | resolveExpr (Expr.Call {callee, arguments, ...}, ss) =
        foldl resolveExpr (resolveExpr (callee, ss)) arguments
    | resolveExpr (Expr.Get (expr, _), ss) = resolveExpr (expr, ss)
    | resolveExpr (Expr.Unary (_, right), ss) = resolveExpr (right, ss)
    | resolveExpr (Expr.Binary (left, _, right), ss) =
        resolveExpr (right, resolveExpr (left, ss))
    | resolveExpr (Expr.Logical (left, _, right), ss) =
        resolveExpr (right, resolveExpr (left, ss))
    | resolveExpr (Expr.Assign (_, value), ss) = resolveExpr (value, ss)

  and resolve stmts =
    ignore (resolveStmts (stmts, [], FunctionType.NONE))

  and resolveStmts (stmts, scopes, func) =
    foldl (resolveStmt func) scopes stmts
end
