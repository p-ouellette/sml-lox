structure Environment :>
sig
  type t

  val empty: t
  val define: t * string * LoxValue.t -> t
  val assign: t * SourceToken.t * LoxValue.t -> t
  val get: t * SourceToken.t -> LoxValue.t
end =
struct
  structure M =
    RedBlackMapFn
      (struct type ord_key = string val compare = String.compare end)

  type t = LoxValue.t M.map

  val empty = M.empty

  fun define (env, name, value) = M.insert (env, name, value)

  fun undefined name =
    raise Error.RuntimeError
      (name, "Undefined variable '" ^ #lexeme name ^ "'.")

  fun assign (env, name as {lexeme, ...}, value) =
    if M.inDomain (env, lexeme) then M.insert (env, lexeme, value)
    else undefined name

  fun get (env, name) =
    case M.find (env, #lexeme name) of
      SOME value => value
    | NONE => undefined name
end
