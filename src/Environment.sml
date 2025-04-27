structure Environment :>
sig
  (* The type of an environment mapping identifiers to values of type 'a. *)
  type 'a t

  val new: 'a t option -> 'a t
  val enclosing: 'a t -> 'a t
  val level: 'a t -> int
  val define: 'a t * string * 'a -> 'a t
  val assign: 'a t * SourceToken.t * 'a -> 'a t
  val get: 'a t * SourceToken.t -> 'a
end =
struct
  structure M = StringMap

  (* The global environment is stored in a mutable reference because a function
   * in the global scope must have access to variables declared after it.
   *)
  type 'a map = 'a ref M.map
  datatype 'a t =
    Outermost of 'a map ref
  | Inner of {values: 'a map, enclosing: 'a t}

  fun new NONE =
        Outermost (ref M.empty)
    | new (SOME enclosing) = Inner {values = M.empty, enclosing = enclosing}

  fun enclosing (Inner env) = #enclosing env
    | enclosing _ = raise Fail "no enclosing environment"

  fun level (Outermost _) = 0
    | level (Inner env) =
        1 + level (#enclosing env)

  fun define (env as Outermost values, name, value) =
        (values := M.insert (!values, name, ref value); env)
    | define (Inner {values, enclosing}, name, value) =
        Inner
          {values = M.insert (values, name, ref value), enclosing = enclosing}

  fun undefined name =
    raise Error.RuntimeError
      (name, "Undefined variable '" ^ #lexeme name ^ "'.")

  fun getRef (Outermost values, name) =
        (case M.find (!values, #lexeme name) of
           SOME v => v
         | NONE => undefined name)
    | getRef (Inner {values, enclosing}, name) =
        (case M.find (values, #lexeme name) of
           SOME v => v
         | NONE => getRef (enclosing, name))

  fun assign (env, name, value) =
    (getRef (env, name) := value; env)

  fun get (env, name) =
    !(getRef (env, name))
end
