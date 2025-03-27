structure Environment :>
sig
  type t

  val global: unit -> t
  val new: t -> t
  val enclosing: t -> t
  val level: t -> int
  val define: t * string * LoxValue.t -> t
  val assign: t * SourceToken.t * LoxValue.t -> t
  val get: t * SourceToken.t -> LoxValue.t
  val dump: t -> unit
end =
struct
  structure LV = LoxValue
  structure M = StringMap

  (* The global environment is stored in a mutable reference because a function
   * in the global scope must have access to variables declared after it.
   *)
  type map = LV.t ref M.map
  datatype t = Outermost of map ref | Inner of map * t

  val clock = LV.Function
    { arity = 0
    , call = fn _ =>
        LV.Number (Real.fromLargeInt (Time.toSeconds (Time.now ())))
    , repr = "<native fn>"
    }
  val builtins = M.singleton ("clock", ref clock)

  fun global () =
    Outermost (ref builtins)

  fun new enclosing = Inner (M.empty, enclosing)

  fun enclosing (Inner (_, env)) = env
    | enclosing _ = raise Fail "no enclosing environment"

  fun level (Outermost _) = 0
    | level (Inner (_, env)) = 1 + level env

  fun define (env as Outermost values, name, value) =
        (values := M.insert (!values, name, ref value); env)
    | define (Inner (values, enclosing), name, value) =
        Inner (M.insert (values, name, ref value), enclosing)

  fun undefined name =
    raise Error.RuntimeError
      (name, "Undefined variable '" ^ #lexeme name ^ "'.")

  fun getRef (Outermost values, name) =
        (case M.find (!values, #lexeme name) of
           SOME v => v
         | NONE => undefined name)
    | getRef (Inner (values, enclosing), name) =
        (case M.find (values, #lexeme name) of
           SOME v => v
         | NONE => getRef (enclosing, name))

  fun assign (env, name, value) =
    (getRef (env, name) := value; env)

  fun get (env, name) =
    !(getRef (env, name))

  fun dump env =
    let
      val values =
        case env of
          Outermost values => !values
        | Inner (values, _) => values
      val indent = implode (List.tabulate (level env * 2, fn _ => #" "))
    in
      case env of
        Outermost _ => ()
      | Inner (_, env) => dump env;
      print (indent ^ "level " ^ Int.toString (level env) ^ "\n");
      M.appi (fn (k, v) => print (indent ^ k ^ " = " ^ LV.toString (!v) ^ "\n"))
        values
    end
end
