structure Interpreter:
sig
  val interpret: Stmt.t list -> unit
end =
struct
  structure T = Token
  structure LV = LoxValue

  fun numberOperand (_, LV.Number n) = n
    | numberOperand (operator, _) =
        raise Error.RuntimeError (operator, "Operand must be a number.")

  fun numberOperands (_, LV.Number x, LV.Number y) = (x, y)
    | numberOperands (operator, _, _) =
        raise Error.RuntimeError (operator, "Operands must be numbers.")

  fun execute (Stmt.Expression expr) =
        ignore (evaluate expr)
    | execute (Stmt.Print expr) =
        print (LV.toString (evaluate expr) ^ "\n")

  and evaluate Expr.Nil = LV.Nil
    | evaluate (Expr.Boolean b) = LV.Boolean b
    | evaluate (Expr.Number n) = LV.Number n
    | evaluate (Expr.String s) = LV.String s
    | evaluate (Expr.Grouping expr) = evaluate expr
    | evaluate (Expr.Unary x) = unaryExpr x
    | evaluate (Expr.Binary x) = binaryExpr x

  and unaryExpr (operator, right) =
    let
      val right = evaluate right
    in
      case #token operator of
        T.MINUS => LV.Number (~(numberOperand (operator, right)))
      | T.BANG => LV.Boolean (not (LV.isTruthy right))
      | _ => raise Fail "impossible"
    end

  and binaryExpr (left, operator, right) =
    let
      val left = evaluate left
      val right = evaluate right
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

  fun interpret statements =
    app execute statements
    handle Error.RuntimeError err => Error.runtimeError err
end
