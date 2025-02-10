structure LoxValue =
struct
  datatype t = String of string | Number of real | Boolean of bool | Nil

  fun isTruthy (Boolean false) = false
    | isTruthy Nil = false
    | isTruthy _ = true

  fun toString (String s) = s
    | toString (Number n) = Real.toString n
    | toString (Boolean b) = Bool.toString b
    | toString Nil = "nil"
end

structure Interpreter =
struct
  structure T = Token

  fun checkNumberOperand (_, LoxValue.Number n) = n
    | checkNumberOperand (operator, _) =
        raise Error.RuntimeError (operator, "Operand must be a number.")

  fun evaluate (Expr.String s) = LoxValue.String s
    | evaluate (Expr.Number n) = LoxValue.Number n
    | evaluate (Expr.Boolean b) = LoxValue.Boolean b
    | evaluate Expr.Nil = LoxValue.Nil
    | evaluate (Expr.Unary x) = evalUnaryExpr x
    | evaluate _ = raise Fail "nyi"

  and evalUnaryExpr (st, right) =
    let
      val right = evaluate right
    in
      case #token st of
        T.MINUS => LoxValue.Number (~(checkNumberOperand (st, right)))
      | T.BANG => LoxValue.Boolean (not (LoxValue.isTruthy right))
      | _ => raise Fail "impossible"
    end

  fun interpret expr =
    let val value = evaluate expr
    in print (LoxValue.toString value ^ "\n")
    end
end
