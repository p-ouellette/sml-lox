structure Interpreter:
sig
  val interpret: Stmt.t list * Environment.t -> Environment.t
end =
struct
  structure T = Token
  structure LV = LoxValue
  structure Env = Environment

  fun numberOperand (_, LV.Number n) = n
    | numberOperand (operator, _) =
        raise Error.RuntimeError (operator, "Operand must be a number.")

  fun numberOperands (_, LV.Number x, LV.Number y) = (x, y)
    | numberOperands (operator, _, _) =
        raise Error.RuntimeError (operator, "Operands must be numbers.")

  fun execute (Stmt.Expression expr, env) =
        (evaluate (env, expr); env)
    | execute (Stmt.Print expr, env) =
        (print (LV.toString (evaluate (env, expr)) ^ "\n"); env)
    | execute (Stmt.Var ({lexeme, ...}, init), env) =
        Env.define (env, lexeme, evaluate (env, init))

  and evaluate (env, expr) =
    let
      fun eval Expr.Nil = LV.Nil
        | eval (Expr.Boolean b) = LV.Boolean b
        | eval (Expr.Number n) = LV.Number n
        | eval (Expr.String s) = LV.String s
        | eval (Expr.Variable name) = Env.get (env, name)
        | eval (Expr.Grouping expr) = evaluate (env, expr)
        | eval (Expr.Unary x) = unaryExpr x env
        | eval (Expr.Binary x) = binaryExpr x env
    in
      eval expr
    end

  and unaryExpr (operator, right) env =
    let
      val right = evaluate (env, right)
    in
      case #token operator of
        T.MINUS => LV.Number (~(numberOperand (operator, right)))
      | T.BANG => LV.Boolean (not (LV.isTruthy right))
      | _ => raise Fail "impossible"
    end

  and binaryExpr (left, operator, right) env =
    let
      val left = evaluate (env, left)
      val right = evaluate (env, right)
    in
      case #token operator of
        T.MINUS => LV.Number (op- (numberOperands (operator, left, right)))
      | T.SLASH => LV.Number (op/ (numberOperands (operator, left, right)))
      | T.STAR => LV.Number (op* (numberOperands (operator, left, right)))
      | T.PLUS =>
          (case (left, right) of
             (LV.Number x, LV.Number y) => LV.Number (x + y)
           | (LV.String x, LV.String y) => LV.String (x ^ y)
           | _ =>
               raise Error.RuntimeError
                 (operator, "Operands must be two numbers or two strings."))
      | _ => raise Fail "impossible"
    end

  fun interpret (statements, env) =
    foldl execute env statements
    handle Error.RuntimeError err => (Error.runtimeError err; env)
end
