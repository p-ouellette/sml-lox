structure Interpreter:
sig
  val interpret: Expr.t -> unit
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

  fun evaluate Expr.Nil = LV.Nil
    | evaluate (Expr.Boolean b) = LV.Boolean b
    | evaluate (Expr.Number n) = LV.Number n
    | evaluate (Expr.String s) = LV.String s
    | evaluate (Expr.Grouping expr) = evaluate expr
    | evaluate (Expr.Unary x) = evalUnaryExpr x
    | evaluate (Expr.Binary x) = evalBinaryExpr x

  and evalUnaryExpr (operator, right) =
    let
      val right = evaluate right
    in
      case #token operator of
        T.MINUS => LV.Number (~(numberOperand (operator, right)))
      | T.BANG => LV.Boolean (not (LV.isTruthy right))
      | _ => raise Fail "impossible"
    end

  and evalBinaryExpr (left, operator, right) =
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

  fun interpret expr =
    print (LV.toString (evaluate expr) ^ "\n")
    handle Error.RuntimeError err => Error.runtimeError err
end
