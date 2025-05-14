structure Value:
sig
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of {repr: string, arity: int, call: t list -> t}
  | Function of {declaration: Stmt.function, closure: t Environment.t}
  | Class of class
  | Instance of {class: class, fields: t StringMap.map} ref
  withtype class =
    { name: string
    , methods: {declaration: Stmt.function, closure: t Environment.t} StringMap.map
    }

  type function = {declaration: Stmt.function, closure: t Environment.t}
  type instance = {class: class, fields: t StringMap.map} ref

  structure Function:
  sig
    val arity: function -> int
    val bind: function * instance -> function
  end

  structure Class:
  sig
    val findMethod: class * string -> function option
    val arity: class -> int
  end

  structure Instance:
  sig
    val new: class -> instance
    val get: instance * SourceToken.t -> t
    val set: instance * SourceToken.t * t -> unit
  end

  val isTruthy: t -> bool
  val isEqual: t * t -> bool
  val toString: t -> string
end =
struct
  structure Env = Environment

  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of {repr: string, arity: int, call: t list -> t}
  | Function of {declaration: Stmt.function, closure: t Env.t}
  | Class of class
  | Instance of {class: class, fields: t StringMap.map} ref
  withtype class =
    { name: string
    , methods: {declaration: Stmt.function, closure: t Env.t} StringMap.map
    }

  type function = {declaration: Stmt.function, closure: t Env.t}
  type instance = {class: class, fields: t StringMap.map} ref

  structure Function =
  struct
    fun arity (f: function) =
      length (#params (#declaration f))

    fun bind ({declaration, closure}, instance) =
      let
        val env = Env.new (SOME closure)
        val env = Env.define (env, "this", Instance instance)
      in
        {declaration = declaration, closure = env}
      end
  end

  structure Class =
  struct
    fun findMethod (class: class, name) =
      StringMap.find (#methods class, name)

    fun arity class =
      case findMethod (class, "init") of
        SOME init => Function.arity init
      | NONE => 0
  end

  structure Instance =
  struct
    fun new class = ref {class = class, fields = StringMap.empty}

    fun get (instance as ref {class, fields}, name) =
      case StringMap.find (fields, #lexeme name) of
        SOME value => value
      | NONE =>
          (case Class.findMethod (class, #lexeme name) of
             SOME method => Function (Function.bind (method, instance))
           | NONE =>
               raise Error.RuntimeError
                 (name, "Undefined property '" ^ #lexeme name ^ "'."))

    fun set (instance as ref {class, fields}, name: SourceToken.t, value) =
      let val fields = StringMap.insert (fields, #lexeme name, value)
      in instance := {class = class, fields = fields}
      end
  end

  fun isTruthy Nil = false
    | isTruthy (Boolean false) = false
    | isTruthy _ = true

  fun isEqual (Nil, Nil) = true
    | isEqual (Boolean a, Boolean b) = a = b
    | isEqual (Number a, Number b) = Real.== (a, b)
    | isEqual (String a, String b) = a = b
    | isEqual (Instance a, Instance b) = a = b
    | isEqual _ = false

  fun numberToString n =
    (* MLton Real.fmt ignores the sign of 0 *)
    if Real.== (n, 0.0) andalso Real.signBit n then
      "-0"
    else
      let
        val s = Real.fmt (StringCvt.FIX (SOME 6)) n
        val s = String.map (fn #"~" => #"-" | c => c) s
        val (i, frac) =
          case String.fields (fn #"." => true | _ => false) s of
            [a, b] => (a, b)
          | _ => raise Fail "impossible"
        val frac = StringCvt.dropl (fn #"0" => true | _ => false) List.getItem
          (rev (explode frac))
      in
        if null frac then i else i ^ "." ^ (implode (rev frac))
      end

  fun toString Nil = "nil"
    | toString (Boolean b) = Bool.toString b
    | toString (Number n) = numberToString n
    | toString (String s) = s
    | toString (Builtin b) = #repr b
    | toString (Function f) =
        "<fn " ^ #lexeme (#name (#declaration f)) ^ ">"
    | toString (Class c) = #name c
    | toString (Instance i) =
        #name (#class (!i)) ^ " instance"
end
