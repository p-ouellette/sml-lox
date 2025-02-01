structure Scanner :>
sig
  val scanTokens: string -> Token.t list
end =
struct
  structure T = Token

  (* character source *)
  type cs = {source: string, current: int, line: int}

  fun incCurrent {source, current, line} =
    {source = source, current = current + 1, line = line}

  fun incLine {source, current, line} =
    {source = source, current = current, line = line + 1}

  fun getc (cs as {source, current, ...}) =
    if current >= size source then NONE
    else SOME (String.sub (source, current), incCurrent cs)

  fun newToken ({source, current, line}, start, ty) =
    { ty = ty
    , lexeme = String.substring (source, start, current - start)
    , line = line
    }

  fun scanToken (cs as {current = start, line, ...}) =
    let
      fun token (cs, ty) =
        SOME (newToken (cs, start, ty), cs)

      fun match (cs, expected, ok, fail) =
        case getc cs of
          SOME (c, cs') =>
            if c = expected then token (cs', ok) else token (cs, fail)
        | _ => token (cs, fail)

      fun comment cs =
        case getc cs of
          SOME (c, cs') => if c = #"\n" then scanToken cs else comment cs'
        | NONE => NONE

      fun matchSlash cs =
        case getc cs of
          SOME (#"/", cs') => comment cs'
        | _ => token (cs, T.SLASH)

      fun string (cs as {source, current, line}, len) =
        case getc cs of
          SOME (#"\"", cs') =>
            let val s = String.substring (source, current - len, len)
            in token (cs', T.STRING s)
            end
        | SOME (c, cs') =>
            string (if c = #"\n" then incLine cs' else cs', len + 1)
        | NONE => (Error.error (line, "Unterminated string."); NONE)

      (* TODO: test this, can we use dropl elsewhere? *)
      fun number (cs as {source, current, ...}) =
        let
          val cs = StringCvt.dropl Char.isDigit getc cs
          val cs as {current = next, ...} =
            case getc cs of
              SOME (#".", cs') => StringCvt.dropl Char.isDigit getc cs'
            | _ => cs
          val n = valOf (Real.fromString (String.substring
            (source, current - 1, next - (current - 1))))
        in
          token (cs, T.NUMBER n)
        end

      val scan =
        fn (#"(", cs) => token (cs, T.LEFT_PAREN)
         | (#")", cs) => token (cs, T.RIGHT_PAREN)
         | (#"{", cs) => token (cs, T.LEFT_BRACE)
         | (#"}", cs) => token (cs, T.RIGHT_BRACE)
         | (#",", cs) => token (cs, T.COMMA)
         | (#".", cs) => token (cs, T.DOT)
         | (#"-", cs) => token (cs, T.MINUS)
         | (#"+", cs) => token (cs, T.PLUS)
         | (#";", cs) => token (cs, T.SEMICOLON)
         | (#"*", cs) => token (cs, T.STAR)
         | (#"!", cs) => match (cs, #"=", T.BANG_EQUAL, T.BANG)
         | (#"=", cs) => match (cs, #"=", T.EQUAL_EQUAL, T.EQUAL)
         | (#"<", cs) => match (cs, #"=", T.LESS_EQUAL, T.LESS)
         | (#">", cs) => match (cs, #"=", T.GREATER_EQUAL, T.GREATER)
         | (#"/", cs) => matchSlash cs
         | (#" ", cs) => scanToken cs
         | (#"\r", cs) => scanToken cs
         | (#"\t", cs) => scanToken cs
         | (#"\n", cs) => scanToken (incLine cs)
         | (#"\"", cs) => string (cs, 0)
         | (c, cs) =>
          if Char.isDigit c then
            number cs
          else
            (Error.error (line, "Unexpected character: " ^ str c); scanToken cs)
    in
      Option.mapPartial scan (getc cs)
    end

  fun scanTokens source =
    let
      fun scan (l, cs as {line, ...}) =
        case scanToken cs of
          NONE => rev ({ty = Token.EOF, lexeme = "", line = line} :: l)
        | SOME (t, cs) => scan (t :: l, cs)
    in
      scan ([], {source = source, current = 0, line = 1})
    end
end
