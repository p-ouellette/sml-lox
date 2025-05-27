structure Value:
sig
  type builtin
  type function
  type class
  type instance

  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of builtin
  | Function of function
  | Class of class
  | Instance of instance

  val isTruthy: t -> bool
  val isEqual: t * t -> bool

  structure Builtin:
  sig
    val new: {arity: int, call: t list -> t} -> builtin
    val arity: builtin -> int
    val call: builtin -> t list -> t
    val toString: builtin -> string
  end

  structure Function:
  sig
    val new:
      { declaration: Stmt.function
      , closure: t Environment.t
      , isInitializer: bool
      }
      -> function
    val name: function -> SourceToken.t
    val params: function -> SourceToken.t list
    val body: function -> Stmt.t list
    val closure: function -> t Environment.t
    val isInitializer: function -> bool
    val arity: function -> int
    val bind: function * instance -> function
    val toString: function -> string
  end

  structure Class:
  sig
    val new:
      {name: string, superclass: class option, methods: function StringMap.map}
      -> class
    val name: class -> string
    val findMethod: class * string -> function option
    val arity: class -> int
    val toString: class -> string
  end

  structure Instance:
  sig
    val new: class -> instance
    val get: instance * SourceToken.t -> t
    val set: instance * SourceToken.t * t -> unit
    val toString: instance -> string
  end
end =

struct
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of builtin
  | Function of function
  | Class of class
  | Instance of instance
  and builtin =
    B of {arity: int, call: t list -> t} ref
  and function =
    F of
      { declaration: Stmt.function
      , closure: t Environment.t
      , isInitializer: bool
      } ref
  and class =
    C of
      {name: string, superclass: class option, methods: function StringMap.map} ref
  and instance =
    I of {class: class, fields: t StringMap.map} ref

  fun isTruthy Nil = false
    | isTruthy (Boolean false) = false
    | isTruthy _ = true

  fun isEqual (Nil, Nil) = true
    | isEqual (Boolean a, Boolean b) = a = b
    | isEqual (Number a, Number b) = Real.== (a, b)
    | isEqual (String a, String b) = a = b
    | isEqual (Builtin a, Builtin b) = a = b
    | isEqual (Function a, Function b) = a = b
    | isEqual (Class a, Class b) = a = b
    | isEqual (Instance a, Instance b) = a = b
    | isEqual _ = false

  structure Builtin =
  struct
    fun new x =
      B (ref x)

    fun arity (B x) =
      #arity (!x)

    fun call (B x) =
      #call (!x)

    fun toString _ = "<native fn>"
  end

  structure Function =
  struct
    fun new x =
      F (ref x)

    fun declaration (F x) =
      #declaration (!x)

    fun name f =
      #name (declaration f)

    fun params f =
      #params (declaration f)

    fun body f =
      #body (declaration f)

    fun closure (F x) =
      #closure (!x)

    fun isInitializer (F x) =
      #isInitializer (!x)

    fun arity (F x) =
      length (#params (#declaration (!x)))

    fun bind (method, instance) =
      let
        val env = Environment.new (SOME (closure method))
        val env = Environment.define (env, "this", Instance instance)
      in
        new
          { declaration = declaration method
          , closure = env
          , isInitializer = isInitializer method
          }
      end

    fun toString f =
      "<fn " ^ #lexeme (name f) ^ ">"
  end

  structure Class =
  struct
    fun new x =
      C (ref x)

    fun name (C x) =
      #name (!x)

    fun superclass (C x) =
      #superclass (!x)

    fun methods (C x) =
      #methods (!x)

    fun findMethod (class, name) =
      case StringMap.find (methods class, name) of
        SOME method => SOME method
      | NONE =>
          Option.mapPartial (fn c => findMethod (c, name)) (superclass class)

    fun arity class =
      case findMethod (class, "init") of
        SOME init => Function.arity init
      | NONE => 0

    fun toString class = name class
  end

  structure Instance =
  struct
    fun new class =
      I (ref {class = class, fields = StringMap.empty})

    fun class (I x) =
      #class (!x)

    fun fields (I x) =
      #fields (!x)

    fun get (instance, name) =
      case StringMap.find (fields instance, #lexeme name) of
        SOME value => value
      | NONE =>
          (case Class.findMethod (class instance, #lexeme name) of
             SOME method => Function (Function.bind (method, instance))
           | NONE =>
               raise Error.RuntimeError
                 (name, "Undefined property '" ^ #lexeme name ^ "'."))

    fun set (instance as I x, name: SourceToken.t, value) =
      let val fields = StringMap.insert (fields instance, #lexeme name, value)
      in x := {class = class instance, fields = fields}
      end

    fun toString instance =
      Class.name (class instance) ^ " instance"
  end
end
