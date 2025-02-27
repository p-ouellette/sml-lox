structure LoxValue:
sig
  (* The type of a Lox value, parameterized with the type an environment. *)
  datatype 'a t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  (* `call (args, env)` returns the value produced by the call.
   * `env` is the environment before the call.
   *)
  | Callable of {arity: int, call: 'a t list * 'a -> 'a t, repr: string}

  val isTruthy: 'a t -> bool
  val isEqual: 'a t * 'a t -> bool
  val toString: 'a t -> string
end =
struct
  datatype 'a t =
    Nil
  | Boolean of bool
  | Number of real
  | String of string
  | Callable of {arity: int, call: 'a t list * 'a -> 'a t, repr: string}

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
    | toString (Callable f) = #repr f
end
