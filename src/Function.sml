structure Function:
sig
  val new:
    { declaration: Stmt.function
    , closure: Value.t Environment.t
    , isInitializer: bool
    }
    -> Value.function
  val name: Value.function -> SourceToken.t
  val params: Value.function -> SourceToken.t list
  val body: Value.function -> Stmt.t list
  val closure: Value.function -> Value.t Environment.t
  val isInitializer: Value.function -> bool
  val arity: Value.function -> int
  val bind: Value.function * Value.instance -> Value.function
  val toString: Value.function -> string
end =
struct
  fun new x = ref x

  fun name (f: Value.function) =
    #name (#declaration (!f))

  fun params (f: Value.function) =
    #params (#declaration (!f))

  fun body (f: Value.function) =
    #body (#declaration (!f))

  fun closure (f: Value.function) =
    #closure (!f)

  fun isInitializer (f: Value.function) =
    #isInitializer (!f)

  fun arity (f: Value.function) =
    length (#params (#declaration (!f)))

  fun bind (ref {declaration, closure, isInitializer}, instance) =
    let
      val env = Environment.new (SOME closure)
      val env = Environment.define (env, "this", Value.Instance instance)
    in
      new
        { declaration = declaration
        , closure = env
        , isInitializer = isInitializer
        }
    end

  fun toString (f: Value.function) =
    "<fn " ^ #lexeme (name f) ^ ">"
end
