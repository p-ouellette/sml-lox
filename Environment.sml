structure Environment :>
sig
  type t

  val empty: t
  val define: t * string * LoxValue.t -> t
  val get: t * SourceToken.t -> LoxValue.t
end =
struct
  structure M =
    RedBlackMapFn
      (struct type ord_key = string val compare = String.compare end)

  type t = LoxValue.t M.map

  val empty = M.empty

  fun define (env, name, value) = M.insert (env, name, value)

  fun get (env, token as {lexeme, ...}) =
    case M.find (env, lexeme) of
      SOME value => value
    | NONE =>
        raise Error.RuntimeError (token, "Undefined variable '" ^ lexeme ^ "'.")
end
