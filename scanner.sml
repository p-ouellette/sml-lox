structure Scanner:
sig
  val scanTokens: string -> Token.t list
end =
struct
  structure T = Token

  (* character source *)
  type cs = {source: string, current: int, line: int}

  fun substr (s, i, j) =
    String.substring (s, i, j - i)

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

  fun newToken ({source, current, line}, start, ty) =
    {ty = ty, lexeme = substr (source, start, current), line = line}

  val keywordType =
    fn "and" => T.AND
     | "class" => T.CLASS
     | "else" => T.ELSE
     | "false" => T.FALSE
     | "for" => T.FOR
     | "fun" => T.FUN
     | "if" => T.FUN
     | "nil" => T.NIL
     | "or" => T.OR
     | "print" => T.PRINT
     | "return" => T.RETURN
     | "super" => T.SUPER
     | "this" => T.THIS
     | "true" => T.TRUE
     | "var" => T.VAR
     | "while" => T.WHILE
     | _ => T.IDENTIFIER

  fun scanToken (cs as {source, current = start, line}) =
    let
      fun token (cs, ty) =
        SOME (newToken (cs, start, ty), cs)

      fun match (cs, expected, ok, fail) =
        case getc cs of
          SOME (c, cs') =>
            if c = expected then token (cs', ok) else token (cs, fail)
        | _ => token (cs, fail)

      fun slash cs =
        case getc cs of
          SOME (#"/", cs') => scanToken (dropl (fn c => c <> #"\n") cs')
        | _ => token (cs, T.SLASH)

      fun string cs =
        case getc cs of
          SOME (#"\"", cs') =>
            token (cs', T.STRING (substr (source, start + 1, #current cs)))
        | SOME (_, cs') => string cs'
        | NONE => (Error.error (#line cs, "Unterminated string."); NONE)

      fun number cs =
        let
          val csInt = dropl Char.isDigit cs
          val cs =
            case getc csInt of
              SOME (#".", csDot) =>
                let val csFrac = dropl Char.isDigit csDot
                in if #current csDot = #current csFrac then csInt else csFrac
                end
            | _ => csInt
          val n = valOf (Real.fromString (substr (source, start, #current cs)))
        in
          token (cs, T.NUMBER n)
        end

      fun identifier cs =
        let val cs = dropl (fn c => Char.isAlphaNum c orelse c = #"_") cs
        in token (cs, keywordType (substr (source, start, #current cs)))
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
         | (#"/", cs) => slash cs
         | (#" ", cs) => scanToken cs
         | (#"\r", cs) => scanToken cs
         | (#"\t", cs) => scanToken cs
         | (#"\n", cs) => scanToken cs
         | (#"\"", cs) => string cs
         | (c, cs) =>
          if Char.isDigit c then
            number cs
          else if Char.isAlpha c then
            identifier cs
          else
            (Error.error (line, "Unexpected character: " ^ str c); scanToken cs)
    in
      Option.mapPartial scan (getc cs)
    end

  fun scanTokens source =
    let
      fun scan (l, cs) =
        case scanToken cs of
          NONE => rev ({ty = Token.EOF, lexeme = "", line = #line cs} :: l)
        | SOME (t, cs) => scan (t :: l, cs)
    in
      scan ([], {source = source, current = 0, line = 1})
    end
end
