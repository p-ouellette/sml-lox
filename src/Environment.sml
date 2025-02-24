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

  fun clock (_, env) =
    (LV.Number (Real.fromLargeInt (Time.toSeconds (Time.now ()))), env)
  val clock = LV.Callable {arity = 0, call = clock, repr = "<native fn>"}

  val base = Env {values = M.singleton ("clock", clock), enclosing = NONE}

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

  fun dump (Env {values, enclosing}) =
    ( M.appi
        (fn (name, value) => print (name ^ " = " ^ LV.toString value ^ "\n"))
        values
    ; case enclosing of
        SOME env => dump env
      | NONE => ()
    )
end
