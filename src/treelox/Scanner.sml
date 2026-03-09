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
    fn "and" => T.And
     | "class" => T.Class
     | "else" => T.Else
     | "false" => T.False
     | "for" => T.For
     | "fun" => T.Fun
     | "if" => T.If
     | "nil" => T.Nil
     | "or" => T.Or
     | "print" => T.Print
     | "return" => T.Return
     | "super" => T.Super
     | "this" => T.This
     | "true" => T.True
     | "var" => T.Var
     | "while" => T.While
     | _ => T.Identifier

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
        | _ => token (s, T.Slash)

      fun string s =
        case getc s of
          SOME (#"\"", s') =>
            token (s', T.String (substr (source, start + 1, #current s)))
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
          token (s, T.Number n)
        end

      fun identifier s =
        let val s = dropl (fn c => Char.isAlphaNum c orelse c = #"_") s
        in token (s, keywordType (substr (source, start, #current s)))
        end

      val scan =
        fn (#"(", s) => token (s, T.LeftParen)
         | (#")", s) => token (s, T.RightParen)
         | (#"{", s) => token (s, T.LeftBrace)
         | (#"}", s) => token (s, T.RightBrace)
         | (#",", s) => token (s, T.Comma)
         | (#".", s) => token (s, T.Dot)
         | (#"-", s) => token (s, T.Minus)
         | (#"+", s) => token (s, T.Plus)
         | (#";", s) => token (s, T.Semicolon)
         | (#"*", s) => token (s, T.Star)
         | (#"!", s) => match (s, #"=", T.BangEqual, T.Bang)
         | (#"=", s) => match (s, #"=", T.EqualEqual, T.Equal)
         | (#"<", s) => match (s, #"=", T.LessEqual, T.Less)
         | (#">", s) => match (s, #"=", T.GreaterEqual, T.Greater)
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
          NONE => rev ({token = Token.Eof, lexeme = "", line = #line s} :: l)
        | SOME (t, s) => scan (t :: l, s)
    in
      scan ([], {source = source, current = 0, line = 1})
    end
end
