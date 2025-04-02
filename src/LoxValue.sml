structure LoxValue:
sig
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Function of {arity: int, call: t list -> t, repr: string}
  | Class of class
  | Instance of {class: class, fields: t StringMap.map}
  withtype class =
    { name: string
    , arity: int
    , call: t list -> t
    , methods: {arity: int, call: t list -> t, repr: string} StringMap.map
    }

  type function = {arity: int, call: t list -> t, repr: string}
  type instance = {class: class, fields: t StringMap.map}

  val isTruthy: t -> bool
  val isEqual: t * t -> bool
  val toString: t -> string
  val instanceGet: instance * SourceToken.t -> t
  val instanceSet: instance * SourceToken.t * t -> t
end =
struct
  datatype t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Function of {arity: int, call: t list -> t, repr: string}
  | Class of class
  | Instance of {class: class, fields: t StringMap.map}
  withtype class =
    { name: string
    , arity: int
    , call: t list -> t
    , methods: {arity: int, call: t list -> t, repr: string} StringMap.map
    }

  type function = {arity: int, call: t list -> t, repr: string}
  type instance = {class: class, fields: t StringMap.map}

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
    | toString (Function f) = #repr f
    | toString (Class c) = #name c
    | toString (Instance i) =
        #name (#class i) ^ " instance"

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
end
