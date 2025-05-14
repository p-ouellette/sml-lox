structure Instance:
sig
  val new: Value.class -> Value.instance
  val get: Value.instance * SourceToken.t -> Value.t
  val set: Value.instance * SourceToken.t * Value.t -> unit
  val toString: Value.instance -> string
end =
struct
  fun new class = ref {class = class, fields = StringMap.empty}

  fun get (instance as ref {class, fields}, name) =
    case StringMap.find (fields, #lexeme name) of
      SOME value => value
    | NONE =>
        (case Class.findMethod (class, #lexeme name) of
           SOME method => Value.Function (Function.bind (method, instance))
         | NONE =>
             raise Error.RuntimeError
               (name, "Undefined property '" ^ #lexeme name ^ "'."))

  fun set (instance as ref {class, fields}, name: SourceToken.t, value) =
    let val fields = StringMap.insert (fields, #lexeme name, value)
    in instance := {class = class, fields = fields}
    end

  fun toString (instance: Value.instance) =
    #name (#class (!instance)) ^ " instance"
end
