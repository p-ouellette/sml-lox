structure Interpreter:
sig
  val baseEnv: unit -> Value.t Environment.t
  val interpret: Stmt.t list * Value.t Environment.t -> Value.t Environment.t
end =
struct
  structure T = Token
  structure V = Value
  structure Env = Environment

  exception Return of V.t

  val clock = V.Builtin
    { arity = 0
    , call = fn _ => V.Number (Real.fromLargeInt (Time.toSeconds (Time.now ())))
    }

  fun baseEnv () =
    Env.define (Env.new NONE, "clock", clock)

  fun numberOperand (_, V.Number n) = n
    | numberOperand (operator, _) =
        raise Error.RuntimeError (operator, "Operand must be a number.")

  fun numberOperands (_, V.Number x, V.Number y) = (x, y)
    | numberOperands (operator, _, _) =
        raise Error.RuntimeError (operator, "Operands must be numbers.")

  fun execute (Stmt.Class {name, methods}, env) =
        let
          fun insertMethod (stmt, methods) =
            let val (func, _) = executeFunDecl (stmt, env)
            in StringMap.insert (methods, #lexeme (#name stmt), func)
            end
          val methods = foldl insertMethod StringMap.empty methods
          val class = {name = #lexeme name, methods = methods}
        in
          Env.define (env, #lexeme name, V.Class class)
        end
    | execute (Stmt.Function f, env) =
        let val (_, env) = executeFunDecl (f, env)
        in env
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
          if V.isTruthy cond then
            execute (thenBranch, env)
          else
            getOpt (Option.map (fn stmt => execute (stmt, env)) elseBranch, env)
        end
    | execute (Stmt.Print expr, env) =
        let val (value, env) = evaluate (expr, env)
        in (print (ValueFormat.fmt value ^ "\n"); env)
        end
    | execute (Stmt.Return (_, value), env) =
        let val (value, _) = evaluate (value, env)
        in raise Return value
        end
    | execute (Stmt.While {condition, body}, env) =
        let
          fun loop env =
            let val (cond, env) = evaluate (condition, env)
            in if V.isTruthy cond then loop (execute (body, env)) else env
            end
        in
          loop env
        end
    | execute (Stmt.Block stmts, env) =
        executeBlock (stmts, Env.new (SOME env))

  and executeFunDecl (declaration, env) =
    let
      (* Define the function in the environment for recursive calls. *)
      val env = Env.define (env, #lexeme (#name declaration), V.Nil)
      val function = {declaration = declaration, closure = env}
      (* Complete the function declaration. *)
      val env = Env.assign (env, #name declaration, V.Function function)
    in
      (function, env)
    end

  and executeBlock (stmts, env) =
    Env.enclosing (foldl execute env stmts)

  and evaluate (expr, env) =
    let
      fun eval Expr.Nil = (V.Nil, env)
        | eval (Expr.Boolean b) = (V.Boolean b, env)
        | eval (Expr.Number n) = (V.Number n, env)
        | eval (Expr.String s) = (V.String s, env)
        | eval (Expr.Variable name) = variableExpr (name, env)
        | eval (Expr.This keyword) = variableExpr (keyword, env)
        | eval (Expr.Grouping expr) = evaluate (expr, env)
        | eval (Expr.Call x) = callExpr (x, env)
        | eval (Expr.Get x) = getExpr (x, env)
        | eval (Expr.Unary x) = unaryExpr (x, env)
        | eval (Expr.Binary x) = binaryExpr (x, env)
        | eval (Expr.Logical x) = logicalExpr (x, env)
        | eval (Expr.Set x) = setExpr (x, env)
        | eval (Expr.Assign (name, value)) =
            let val (value, env) = evaluate (value, env)
            in (value, Env.assign (env, name, value))
            end
    in
      eval expr
    end

  and variableExpr (name, env) =
    (Env.get (env, name), env)

  and callExpr ({callee, paren, arguments}, env) =
    let
      fun evalArg (arg, (args, env)) =
        let val (arg, env) = evaluate (arg, env)
        in (arg :: args, env)
        end
      val (callee, env) = evaluate (callee, env)
      val (args, env) = foldl evalArg ([], env) arguments
      val args = rev args
      val (arity, call) =
        case callee of
          V.Builtin {arity, call} => (arity, call)
        | V.Function func => (Function.arity func, callFunction func)
        | V.Class class => (Class.arity class, callClass class)
        | _ =>
            raise Error.RuntimeError
              (paren, "Can only call functions and classes.")
    in
      if arity <> length arguments then
        raise Error.RuntimeError
          ( paren
          , "Expected " ^ Int.toString arity ^ " arguments but got "
            ^ Int.toString (length arguments) ^ "."
          )
      else
        (call args, env)
    end

  and callFunction {declaration, closure} args =
    let
      fun enterParam (param: SourceToken.t, arg, env) =
        Env.define (env, #lexeme param, arg)
      val env = Env.new (SOME closure)
      val env = ListPair.foldlEq enterParam env (#params declaration, args)
    in
      (executeBlock (#body declaration, env); V.Nil)
      handle Return value => value
    end

  and callClass class args =
    let
      val instance = Instance.new class
      fun initialize init =
        (callFunction (Function.bind (init, instance)) args; ())
    in
      Option.app initialize (Class.findMethod (class, "init"));
      V.Instance instance
    end

  and getExpr ((object, name), env) =
    let
      val (object, env) = evaluate (object, env)
    in
      case object of
        V.Instance x => (Instance.get (x, name), env)
      | _ => raise Error.RuntimeError (name, "Only instances have properties.")
    end

  and unaryExpr ((operator, right), env) =
    let
      val (right, env) = evaluate (right, env)
    in
      case #token operator of
        T.MINUS => (V.Number (~(numberOperand (operator, right))), env)
      | T.BANG => (V.Boolean (not (V.isTruthy right)), env)
      | _ => raise Fail "invalid unary operator"
    end

  and binaryExpr ((left, operator, right), env) =
    let
      val (left, env) = evaluate (left, env)
      val (right, env) = evaluate (right, env)
      fun numOperands () = numberOperands (operator, left, right)
    in
      case #token operator of
        T.BANG_EQUAL => (V.Boolean (not (V.isEqual (left, right))), env)
      | T.EQUAL_EQUAL => (V.Boolean (V.isEqual (left, right)), env)
      | T.GREATER => (V.Boolean (op> (numOperands ())), env)
      | T.GREATER_EQUAL => (V.Boolean (op>= (numOperands ())), env)
      | T.LESS => (V.Boolean (op< (numOperands ())), env)
      | T.LESS_EQUAL => (V.Boolean (op<= (numOperands ())), env)
      | T.MINUS => (V.Number (op- (numOperands ())), env)
      | T.SLASH => (V.Number (op/ (numOperands ())), env)
      | T.STAR => (V.Number (op* (numOperands ())), env)
      | T.PLUS =>
          (case (left, right) of
             (V.Number x, V.Number y) => (V.Number (x + y), env)
           | (V.String x, V.String y) => (V.String (x ^ y), env)
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
        T.OR => if V.isTruthy left then (left, env) else evaluate (right, env)
      | T.AND => if V.isTruthy left then evaluate (right, env) else (left, env)
      | _ => raise Fail "invalid logical operator"
    end

  and setExpr ((object, name, value), env) =
    let
      val (object, env) = evaluate (object, env)
      val object =
        case object of
          V.Instance x => x
        | _ => raise Error.RuntimeError (name, "Only instances have fields.")
      val (value, env) = evaluate (value, env)
    in
      Instance.set (object, name, value);
      (value, env)
    end

  fun interpret (statements, env) =
    foldl execute env statements
    handle Error.RuntimeError err => (Error.runtimeError err; env)
end
