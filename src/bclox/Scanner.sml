structure Scanner:
sig
  type cs

  val init: string -> cs
  val scan: cs -> Token.t * cs
end =
struct
  structure TK = Token.Kind

  (* character source *)
  type cs = {source: string, current: int, line: int}

  fun init source = {source = source, current = 0, line = 1}

  fun scan (cs as {source, current, line}) =
    if current >= String.size source then
      ({kind = TK.EOF, lexeme = "", line = line}, cs)
    else
      ({kind = TK.ERROR, lexeme = "Unexpected character", line = line}, cs)
end
