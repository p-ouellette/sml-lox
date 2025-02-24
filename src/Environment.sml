structure Environment :>
sig
  type t

  val base: t
  val new: t -> t
  val enclosing: t -> t
  val define: t * string * t LoxValue.t -> t
  val assign: t * SourceToken.t * t LoxValue.t -> t
  val get: t * SourceToken.t -> t LoxValue.t
  val dump: t -> unit
end =
struct
  structure LV = LoxValue
  structure M =
    RedBlackMapFn
      (struct type ord_key = string val compare = String.compare end)

  datatype t = Env of {values: t LV.t M.map, enclosing: t option}

  fun new enclosing =
    Env {values = M.empty, enclosing = SOME enclosing}

  fun enclosing (Env {enclosing = SOME env, ...}) = env
    | enclosing _ = raise Fail "no enclosing environment"

  fun insert (Env {values, enclosing}, name, value) =
    Env {values = M.insert (values, name, value), enclosing = enclosing}

  val define = insert

  fun undefined name =
    raise Error.RuntimeError
      (name, "Undefined variable '" ^ #lexeme name ^ "'.")

  fun assign (env as Env {values, enclosing}, name as {lexeme, ...}, value) =
    if M.inDomain (values, lexeme) then
      insert (env, lexeme, value)
    else
      case enclosing of
        SOME env =>
          Env {values = values, enclosing = SOME (assign (env, name, value))}
      | NONE => undefined name

  fun get (Env {values, enclosing}, name) =
    case M.find (values, #lexeme name) of
      SOME value => value
    | NONE =>
        (case enclosing of
           SOME env => get (env, name)
         | NONE => undefined name)

  fun dump env =
    let
      fun dump' (Env {values, enclosing}, level) =
        ( case enclosing of
            SOME env => dump' (env, level - 1)
          | NONE => ()
        ; print ("level " ^ Int.toString level ^ "\n")
        ; M.appi
            (fn (name, value) => print (name ^ " = " ^ LV.toString value ^ "\n"))
            values
        )
    in
      dump' (env, 0)
    end

  fun clock (_, env) =
    (LV.Number (Real.fromLargeInt (Time.toSeconds (Time.now ()))), env)
  val clock = LV.Callable {arity = 0, call = clock, repr = "<native fn>"}

  fun env (_, env) =
    (dump env; (LV.Nil, env))
  val env = LV.Callable {arity = 0, call = env, repr = "<native fn>"}

  val globals = [("clock", clock), ("env", env)]
  val base = Env {values = foldl M.insert' M.empty globals, enclosing = NONE}
end
