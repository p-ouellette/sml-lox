structure Scanner:
sig
  type cs

  val init: string -> cs
  val scan: cs -> Token.t * cs
end =
struct
  structure T = Token

  (* character source *)
  type cs = {source: string, current: int, line: int}

  fun init source = {source = source, current = 0, line = 1}

  fun getc {source, current, line} =
    if current >= size source then
      NONE
    else
      let
        val c = String.sub (source, current)
      in
        if c = #"\n" then
          SOME (c, {source = source, current = current + 1, line = line + 1})
        else
          SOME (c, {source = source, current = current + 1, line = line})
      end

  (* advances cs up to the first character not matching the predicate f. *)
  fun dropl f cs =
    StringCvt.dropl f getc cs

  fun makeToken ({source, current, line}, start, kind) =
    { kind = kind
    , lexeme = String.substring (source, start, current - start)
    , line = line
    }

  fun scan (cs as {source = _, current = start, line = _}) =
    let
      fun token (cs, kind) =
        (makeToken (cs, start, kind), cs)

      fun error (cs: cs, message) =
        ({kind = T.ERROR, lexeme = message, line = #line cs}, cs)

      fun match (cs, expected, ok, fail) =
        case getc cs of
          SOME (c, cs') =>
            if c = expected then token (cs', ok) else token (cs, fail)
        | _ => token (cs, fail)

      fun slashOrComment cs =
        case getc cs of
          SOME (#"/", cs') => scan (dropl (fn c => c <> #"\n") cs')
        | _ => token (cs, T.SLASH)

      fun string cs =
        case getc cs of
          SOME (#"\"", cs') => token (cs', T.STRING)
        | SOME (_, cs') => string cs'
        | NONE => error (cs, "Unterminated string.")

      val scan' =
        fn (#"(", cs) => token (cs, T.LEFT_PAREN)
         | (#")", cs) => token (cs, T.RIGHT_PAREN)
         | (#"{", cs) => token (cs, T.LEFT_BRACE)
         | (#"}", cs) => token (cs, T.RIGHT_BRACE)
         | (#";", cs) => token (cs, T.SEMICOLON)
         | (#",", cs) => token (cs, T.COMMA)
         | (#".", cs) => token (cs, T.DOT)
         | (#"-", cs) => token (cs, T.MINUS)
         | (#"+", cs) => token (cs, T.PLUS)
         | (#"*", cs) => token (cs, T.STAR)
         | (#"!", cs) => match (cs, #"=", T.BANG_EQUAL, T.BANG)
         | (#"=", cs) => match (cs, #"=", T.EQUAL_EQUAL, T.EQUAL)
         | (#"<", cs) => match (cs, #"=", T.LESS_EQUAL, T.LESS)
         | (#">", cs) => match (cs, #"=", T.GREATER_EQUAL, T.GREATER)
         | (#"/", cs) => slashOrComment cs
         | (#" ", cs) => scan cs
         | (#"\r", cs) => scan cs
         | (#"\t", cs) => scan cs
         | (#"\n", cs) => scan cs
         | (#"\"", cs) => string cs
         | (_, cs) => error (cs, "Unexpected character.")
    in
      case getc cs of
        NONE => token (cs, T.EOF)
      | SOME (c, cs) => scan' (c, cs)
    end
end
