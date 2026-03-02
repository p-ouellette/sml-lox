structure Scanner:
sig
  val scanTokens: string -> SourceToken.t list
end =
struct
  structure T = Token

  type t = {source: string, current: int, line: int}

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

  (* advances up to the first character not matching the predicate f. *)
  fun dropl f s =
    StringCvt.dropl f getc s

  fun newToken ({source, current, line}, start, token) =
    {token = token, lexeme = substr (source, start, current), line = line}

  val keywordType =
    fn "and" => T.AND
     | "class" => T.CLASS
     | "else" => T.ELSE
     | "false" => T.FALSE
     | "for" => T.FOR
     | "fun" => T.FUN
     | "if" => T.IF
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

  fun scanToken (s as {source, current = start, line}) =
    let
      fun token (s, ty) =
        SOME (newToken (s, start, ty), s)

      fun match (s, expected, ok, fail) =
        case getc s of
          SOME (c, s') =>
            if c = expected then token (s', ok) else token (s, fail)
        | _ => token (s, fail)

      fun slashOrComment s =
        case getc s of
          SOME (#"/", s') => scanToken (dropl (fn c => c <> #"\n") s')
        | _ => token (s, T.SLASH)

      fun string s =
        case getc s of
          SOME (#"\"", s') =>
            token (s', T.STRING (substr (source, start + 1, #current s)))
        | SOME (_, s') => string s'
        | NONE => (Error.error (#line s, "Unterminated string."); NONE)

      fun number s =
        let
          val sInt = dropl Char.isDigit s
          val s =
            case getc sInt of
              SOME (#".", sDot) =>
                let val sFrac = dropl Char.isDigit sDot
                in if #current sDot = #current sFrac then sInt else sFrac
                end
            | _ => sInt
          val n = valOf (Real.fromString (substr (source, start, #current s)))
        in
          token (s, T.NUMBER n)
        end

      fun identifier s =
        let val s = dropl (fn c => Char.isAlphaNum c orelse c = #"_") s
        in token (s, keywordType (substr (source, start, #current s)))
        end

      val scan =
        fn (#"(", s) => token (s, T.LEFT_PAREN)
         | (#")", s) => token (s, T.RIGHT_PAREN)
         | (#"{", s) => token (s, T.LEFT_BRACE)
         | (#"}", s) => token (s, T.RIGHT_BRACE)
         | (#",", s) => token (s, T.COMMA)
         | (#".", s) => token (s, T.DOT)
         | (#"-", s) => token (s, T.MINUS)
         | (#"+", s) => token (s, T.PLUS)
         | (#";", s) => token (s, T.SEMICOLON)
         | (#"*", s) => token (s, T.STAR)
         | (#"!", s) => match (s, #"=", T.BANG_EQUAL, T.BANG)
         | (#"=", s) => match (s, #"=", T.EQUAL_EQUAL, T.EQUAL)
         | (#"<", s) => match (s, #"=", T.LESS_EQUAL, T.LESS)
         | (#">", s) => match (s, #"=", T.GREATER_EQUAL, T.GREATER)
         | (#"/", s) => slashOrComment s
         | (#" ", s) => scanToken s
         | (#"\r", s) => scanToken s
         | (#"\t", s) => scanToken s
         | (#"\n", s) => scanToken s
         | (#"\"", s) => string s
         | (c, s) =>
          if Char.isDigit c then number s
          else if Char.isAlpha c then identifier s
          else (Error.error (line, "Unexpected character."); scanToken s)
    in
      Option.mapPartial scan (getc s)
    end

  fun scanTokens source =
    let
      fun scan (l, s) =
        case scanToken s of
          NONE => rev ({token = Token.EOF, lexeme = "", line = #line s} :: l)
        | SOME (t, s) => scan (t :: l, s)
    in
      scan ([], {source = source, current = 0, line = 1})
    end
end
