structure Interpreter:
sig
  val interpret: Stmt.t list * Environment.t -> Environment.t
end =
struct
  structure T = Token
  structure LV = LoxValue
  structure Env = Environment

  exception Return of LV.t

  fun numberOperand (_, LV.Number n) = n
    | numberOperand (operator, _) =
        raise Error.RuntimeError (operator, "Operand must be a number.")

  fun numberOperands (_, LV.Number x, LV.Number y) = (x, y)
    | numberOperands (operator, _, _) =
        raise Error.RuntimeError (operator, "Operands must be numbers.")

  fun execute (Stmt.Function {name, params, body}, env) =
        let
          val thunk = ref (fn _ => raise Fail "impossible")
          val function = LV.Callable
            { arity = length params
            , call = fn x => !thunk x
            , repr = "<fn " ^ #lexeme name ^ ">"
            }
          val env = Env.define (env, #lexeme name, function)
          fun call args =
            let
              val env =
                ListPair.foldlEq
                  (fn (param, arg, env) => Env.define (env, #lexeme param, arg))
                  (Env.new env) (params, args)
            in
              (executeBlock (body, env); LV.Nil)
              handle Return value => value
            end
        in
          thunk := call;
          env
        end
    | execute (Stmt.Var {name, initializer}, env) =
        let val (value, env) = evaluate (initializer, env)
        in Env.define (env, #lexeme name, value)
        end
    | execute (Stmt.Expression expr, env) =
        let val (_, env) = evaluate (expr, env)
        in env
        end
    | execute (Stmt.If {condition, thenBranch, elseBranch}, env) =
        let
          val (cond, env) = evaluate (condition, env)
        in
          if LV.isTruthy cond then
            execute (thenBranch, env)
          else
            getOpt (Option.map (fn stmt => execute (stmt, env)) elseBranch, env)
        end
    | execute (Stmt.Print expr, env) =
        let val (value, env) = evaluate (expr, env)
        in (print (LV.toString value ^ "\n"); env)
        end
    | execute (Stmt.Return (_, value), env) =
        let val (value, _) = evaluate (value, env)
        in raise Return value
        end
    | execute (Stmt.While {condition, body}, env) =
        let
          fun loop env =
            let val (cond, env) = evaluate (condition, env)
            in if LV.isTruthy cond then loop (execute (body, env)) else env
            end
        in
          loop env
        end
    | execute (Stmt.Block stmts, env) =
        executeBlock (stmts, Env.new env)

  and executeBlock (stmts, env) =
    Env.enclosing (foldl execute env stmts)

  and evaluate (expr, env) =
    let
      fun eval Expr.Nil = (LV.Nil, env)
        | eval (Expr.Boolean b) = (LV.Boolean b, env)
        | eval (Expr.Number n) = (LV.Number n, env)
        | eval (Expr.String s) = (LV.String s, env)
        | eval (Expr.Variable name) =
            (Env.get (env, name), env)
        | eval (Expr.Grouping expr) = evaluate (expr, env)
        | eval (Expr.Call x) = callExpr (x, env)
        | eval (Expr.Unary x) = unaryExpr (x, env)
        | eval (Expr.Binary x) = binaryExpr (x, env)
        | eval (Expr.Logical x) = logicalExpr (x, env)
        | eval (Expr.Assign (name, value)) =
            let val (value, env) = evaluate (value, env)
            in (value, Env.assign (env, name, value))
            end
    in
      eval expr
    end

  and callExpr ({callee, paren, arguments}, env) =
    let
      fun evalArg (arg, (args, env)) =
        let val (arg, env) = evaluate (arg, env)
        in (arg :: args, env)
        end
      val (callee, env) = evaluate (callee, env)
      val (args, env) = foldl evalArg ([], env) arguments
      val args = rev args
    in
      case callee of
        LV.Callable {arity, call, ...} =>
          if arity <> length arguments then
            raise Error.RuntimeError
              ( paren
              , "Expected " ^ Int.toString arity ^ " arguments but got "
                ^ Int.toString (length arguments) ^ "."
              )
          else
            (call args, env)
      | _ =>
          raise Error.RuntimeError
            (paren, "Can only call functions and classes.")
    end

  and unaryExpr ((operator, right), env) =
    let
      val (right, env) = evaluate (right, env)
    in
      case #token operator of
        T.MINUS => (LV.Number (~(numberOperand (operator, right))), env)
      | T.BANG => (LV.Boolean (not (LV.isTruthy right)), env)
      | _ => raise Fail "invalid unary operator"
    end

  and binaryExpr ((left, operator, right), env) =
    let
      val (left, env) = evaluate (left, env)
      val (right, env) = evaluate (right, env)
      fun numOperands () = numberOperands (operator, left, right)
    in
      case #token operator of
        T.BANG_EQUAL => (LV.Boolean (not (LV.isEqual (left, right))), env)
      | T.EQUAL_EQUAL => (LV.Boolean (LV.isEqual (left, right)), env)
      | T.GREATER => (LV.Boolean (op> (numOperands ())), env)
      | T.GREATER_EQUAL => (LV.Boolean (op>= (numOperands ())), env)
      | T.LESS => (LV.Boolean (op< (numOperands ())), env)
      | T.LESS_EQUAL => (LV.Boolean (op<= (numOperands ())), env)
      | T.MINUS => (LV.Number (op- (numOperands ())), env)
      | T.SLASH => (LV.Number (op/ (numOperands ())), env)
      | T.STAR => (LV.Number (op* (numOperands ())), env)
      | T.PLUS =>
          (case (left, right) of
             (LV.Number x, LV.Number y) => (LV.Number (x + y), env)
           | (LV.String x, LV.String y) => (LV.String (x ^ y), env)
           | _ =>
               raise Error.RuntimeError
                 (operator, "Operands must be two numbers or two strings."))
      | _ => raise Fail "invalid binary operator"
    end

  and logicalExpr ((left, operator, right), env) =
    let
      val (left, env) = evaluate (left, env)
    in
      case #token operator of
        T.OR => if LV.isTruthy left then (left, env) else evaluate (right, env)
      | T.AND => if LV.isTruthy left then evaluate (right, env) else (left, env)
      | _ => raise Fail "invalid logical operator"
    end

  fun interpret (statements, env) =
    foldl execute env statements
    handle Error.RuntimeError err => (Error.runtimeError err; env)
end
