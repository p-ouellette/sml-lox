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
  | Instance of {class: class, fields: t StringMap.map}
  withtype class =
    { name: string
    , arity: int
    , call: t list -> t
    , methods: {declaration: Stmt.function, closure: t Environment.t} StringMap.map
    }

  type function = {declaration: Stmt.function, closure: t Environment.t}
  type instance = {class: class, fields: t StringMap.map}

  val functionArity: function -> int
  val instanceGet: instance * SourceToken.t -> t
  val instanceSet: instance * SourceToken.t * t -> t
  val isTruthy: t -> bool
  val isEqual: t * t -> bool
  val toString: t -> string
end =
struct
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Builtin of {repr: string, arity: int, call: t list -> t}
  | Function of {declaration: Stmt.function, closure: t Environment.t}
  | Class of class
  | Instance of {class: class, fields: t StringMap.map}
  withtype class =
    { name: string
    , arity: int
    , call: t list -> t
    , methods: {declaration: Stmt.function, closure: t Environment.t} StringMap.map
    }

  type function = {declaration: Stmt.function, closure: t Environment.t}
  type instance = {class: class, fields: t StringMap.map}

  fun functionArity (f: function) =
    length (#params (#declaration f))

  fun instanceGet ({class, fields}: instance, name) =
    case StringMap.find (fields, #lexeme name) of
      SOME value => value
    | NONE =>
        (case StringMap.find (#methods class, #lexeme name) of
           SOME func => Function func
         | NONE =>
             raise Error.RuntimeError
               (name, "Undefined property '" ^ #lexeme name ^ "'."))

  fun instanceSet ({class, fields}, name: SourceToken.t, value) =
    Instance
      {class = class, fields = StringMap.insert (fields, #lexeme name, value)}

  fun isTruthy Nil = false
    | isTruthy (Boolean false) = false
    | isTruthy _ = true

  fun isEqual (Nil, Nil) = true
    | isEqual (Boolean a, Boolean b) = a = b
    | isEqual (Number a, Number b) = Real.== (a, b)
    | isEqual (String a, String b) = a = b
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
        #name (#class i) ^ " instance"
end
