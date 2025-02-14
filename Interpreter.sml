structure Interpreter:
sig
  val interpret: Stmt.t list * Environment.t -> Environment.t
end =
struct
  structure T = Token
  structure LV = LoxValue
  structure Env = Environment

  fun numOperand (_, LV.Number n) = n
    | numOperand (operator, _) =
        raise Error.RuntimeError (operator, "Operand must be a number.")

  fun numOperands (_, LV.Number x, LV.Number y) = (x, y)
    | numOperands (operator, _, _) =
        raise Error.RuntimeError (operator, "Operands must be numbers.")

  fun execute (Stmt.Var ({lexeme, ...}, init), env) =
        let val (value, env) = evaluate (init, env)
        in Env.define (env, lexeme, value)
        end
    | execute (Stmt.Expression expr, env) =
        #2 (evaluate (expr, env))
    | execute (Stmt.Print expr, env) =
        let val (value, env) = evaluate (expr, env)
        in (print (LV.toString value ^ "\n"); env)
        end
    | execute (Stmt.Block stmts, env) =
        (foldl execute env stmts; env)

  and evaluate (expr, env) =
    let
      fun eval Expr.Nil = (LV.Nil, env)
        | eval (Expr.Boolean b) = (LV.Boolean b, env)
        | eval (Expr.Number n) = (LV.Number n, env)
        | eval (Expr.String s) = (LV.String s, env)
        | eval (Expr.Variable name) =
            (Env.get (env, name), env)
        | eval (Expr.Grouping expr) = evaluate (expr, env)
        | eval (Expr.Unary x) = unaryExpr x env
        | eval (Expr.Binary x) = binaryExpr x env
        | eval (Expr.Assign (name, value)) =
            let val (value, env) = evaluate (value, env)
            in (value, Env.assign (env, name, value))
            end
    in
      eval expr
    end

  and unaryExpr (operator, right) env =
    let
      val (right, env) = evaluate (right, env)
    in
      case #token operator of
        T.MINUS => (LV.Number (~(numOperand (operator, right))), env)
      | T.BANG => (LV.Boolean (not (LV.isTruthy right)), env)
      | _ => raise Fail "impossible"
    end

  and binaryExpr (left, operator, right) env =
    let
      val (left, env) = evaluate (left, env)
      val (right, env) = evaluate (right, env)
    in
      case #token operator of
        T.MINUS => (LV.Number (op- (numOperands (operator, left, right))), env)
      | T.SLASH => (LV.Number (op/ (numOperands (operator, left, right))), env)
      | T.STAR => (LV.Number (op* (numOperands (operator, left, right))), env)
      | T.PLUS =>
          (case (left, right) of
             (LV.Number x, LV.Number y) => (LV.Number (x + y), env)
           | (LV.String x, LV.String y) => (LV.String (x ^ y), env)
           | _ =>
               raise Error.RuntimeError
                 (operator, "Operands must be two numbers or two strings."))
      | _ => raise Fail "impossible"
    end

  fun interpret (statements, env) =
    foldl execute env statements
    handle Error.RuntimeError err => (Error.runtimeError err; env)
end
