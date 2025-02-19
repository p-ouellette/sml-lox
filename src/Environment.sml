structure Environment :>
sig
  type t

  val initial: t
  val new: t -> t
  val enclosing: t -> t
  val define: t * string * LoxValue.t -> t
  val assign: t * SourceToken.t * LoxValue.t -> t
  val get: t * SourceToken.t -> LoxValue.t
end =
struct
  structure M =
    RedBlackMapFn
      (struct type ord_key = string val compare = String.compare end)

  datatype t = Env of {values: LoxValue.t M.map, enclosing: t option}

  val initial = Env {values = M.empty, enclosing = NONE}

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
end
