structure ValueFormat:
sig
  val fmt: Value.t -> string
end =
struct
  structure V = Value

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

  fun fmt V.Nil = "nil"
    | fmt (V.Boolean b) = Bool.toString b
    | fmt (V.Number n) = numberToString n
    | fmt (V.String s) = s
    | fmt (V.Builtin b) = #repr b
    | fmt (V.Function f) = Function.toString f
    | fmt (V.Class c) = #name c
    | fmt (V.Instance i) = Instance.toString i
end
