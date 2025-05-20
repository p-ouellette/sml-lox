structure Function:
sig
  val arity: Value.function -> int
  val bind: Value.function * Value.instance -> Value.function
  val toString: Value.function -> string
end =
struct
  fun arity (func: Value.function) =
    length (#params (#declaration (!func)))

  fun bind (ref {declaration, closure, isInitializer}, instance) =
    let
      val env = Environment.new (SOME closure)
      val env = Environment.define (env, "this", Value.Instance instance)
    in
      ref
        { declaration = declaration
        , closure = env
        , isInitializer = isInitializer
        }
    end

  fun toString (func: Value.function) =
    "<fn " ^ #lexeme (#name (#declaration (!func))) ^ ">"
end
