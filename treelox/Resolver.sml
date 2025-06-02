(* This structure only reports errors; it doesn't need to resolve variables like
 * the book does because we use a persistent data structure for environments.
 *)
structure Resolver:
sig
  val resolve: Stmt.t list -> unit
end =
struct
  structure M = StringMap

  structure FunctionType =
  struct datatype t = NONE | FUNCTION | INITIALIZER | METHOD end

  structure ClassType = struct datatype t = NONE | CLASS | SUBCLASS end

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

  fun resolveStmt _ (Stmt.Class {name, superclass, methods}, ss) =
        let
          val class =
            case superclass of
              NONE => ClassType.CLASS
            | SOME st =>
                ( if #lexeme st = #lexeme name then
                    Error.errorAt (st, "A class can't inherit from itself.")
                  else
                    ()
                ; ClassType.SUBCLASS
                )
          fun resolveMethod (method, ss) =
            let
              val func =
                if #lexeme (#name method) = "init" then FunctionType.INITIALIZER
                else FunctionType.METHOD
            in
              resolveFunction {func = func, class = class} (method, ss)
            end
          val ss = define (declare (ss, name), name)
        in
          foldl resolveMethod ss methods
        end
    | resolveStmt {class, ...} (Stmt.Function f, ss) =
        let
          val ss = define (declare (ss, #name f), #name f)
          val ctx = {func = FunctionType.FUNCTION, class = class}
        in
          resolveFunction ctx (f, ss)
        end
    | resolveStmt ctx (Stmt.Var {name, initializer}, ss) =
        let val ss = resolveExpr ctx (initializer, declare (ss, name))
        in define (ss, name)
        end
    | resolveStmt ctx (Stmt.Expression expr, ss) = resolveExpr ctx (expr, ss)
    | resolveStmt ctx (Stmt.If {condition, thenBranch, elseBranch}, ss) =
        let
          val ss = resolveExpr ctx (condition, ss)
          val ss = resolveStmt ctx (thenBranch, ss)
        in
          case elseBranch of
            SOME stmt => resolveStmt ctx (stmt, ss)
          | NONE => ss
        end
    | resolveStmt ctx (Stmt.Print expr, ss) = resolveExpr ctx (expr, ss)
    | resolveStmt ctx (Stmt.Return (token, expr), ss) =
        ( if #func ctx = FunctionType.NONE then
            Error.errorAt (token, "Can't return from top-level code.")
          else
            ()
        ; case expr of
            Expr.Nil => ()
          | _ =>
              if #func ctx = FunctionType.INITIALIZER then
                Error.errorAt
                  (token, "Can't return a value from an initializer.")
              else
                ()
        ; resolveExpr ctx (expr, ss)
        )
    | resolveStmt ctx (Stmt.While {condition, body}, ss) =
        resolveExpr ctx (condition, resolveStmt ctx (body, ss))
    | resolveStmt ctx (Stmt.Block stmts, ss) =
        endScope (resolveStmts (stmts, beginScope ss, ctx))

  and resolveFunction ctx ({params, body, ...}, ss) =
    let
      val ss = beginScope ss
      val ss = foldl (fn (p, ss) => define (declare (ss, p), p)) ss params
      val ss = resolveStmts (body, ss, ctx)
    in
      endScope ss
    end

  and resolveExpr _ (Expr.Nil, ss) = ss
    | resolveExpr _ (Expr.Boolean _, ss) = ss
    | resolveExpr _ (Expr.Number _, ss) = ss
    | resolveExpr _ (Expr.String _, ss) = ss
    | resolveExpr _ (Expr.Variable name, ss as s :: _) =
        ( if M.find (s, #lexeme name) = SOME false then
            Error.errorAt
              (name, "Can't read local variable in its own initializer.")
          else
            ()
        ; ss
        )
    | resolveExpr _ (Expr.Variable _, ss) = ss
    | resolveExpr ctx (Expr.This keyword, ss) =
        ( if #class ctx = ClassType.NONE then
            Error.errorAt (keyword, "Can't use 'this' outside of a class.")
          else
            ()
        ; ss
        )
    | resolveExpr ctx (Expr.Grouping expr, ss) = resolveExpr ctx (expr, ss)
    | resolveExpr ctx (Expr.Super {keyword, ...}, ss) =
        ( case #class ctx of
            ClassType.NONE =>
              Error.errorAt (keyword, "Can't use 'super' outside of a class.")
          | ClassType.CLASS =>
              Error.errorAt
                (keyword, "Can't use 'super' in a class with no superclass.")
          | ClassType.SUBCLASS => ()
        ; ss
        )
    | resolveExpr ctx (Expr.Call {callee, arguments, ...}, ss) =
        foldl (resolveExpr ctx) (resolveExpr ctx (callee, ss)) arguments
    | resolveExpr ctx (Expr.Get (expr, _), ss) = resolveExpr ctx (expr, ss)
    | resolveExpr ctx (Expr.Unary (_, right), ss) = resolveExpr ctx (right, ss)
    | resolveExpr ctx (Expr.Binary (left, _, right), ss) =
        resolveExpr ctx (right, resolveExpr ctx (left, ss))
    | resolveExpr ctx (Expr.Logical (left, _, right), ss) =
        resolveExpr ctx (right, resolveExpr ctx (left, ss))
    | resolveExpr ctx (Expr.Set (object, _, value), ss) =
        resolveExpr ctx (value, resolveExpr ctx (object, ss))
    | resolveExpr ctx (Expr.Assign (_, value), ss) = resolveExpr ctx (value, ss)

  and resolve stmts =
    let val ctx = {func = FunctionType.NONE, class = ClassType.NONE}
    in ignore (resolveStmts (stmts, [], ctx))
    end

  and resolveStmts (stmts, scopes, ctx) =
    foldl (resolveStmt ctx) scopes stmts
end
