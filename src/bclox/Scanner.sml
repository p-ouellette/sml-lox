structure Scanner:
sig
  type t

  val new: string -> t
  val scan: t -> Token.t * t
end =
struct
  structure T = Token

  type t = {source: string, current: int, line: int}

  fun new source = {source = source, current = 0, line = 1}

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

  fun makeToken ({source, current, line}, start, kind) =
    { kind = kind
    , lexeme = String.substring (source, start, current - start)
    , line = line
    }

  val identifierType =
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

  fun scan (s: t) =
    let
      val start = #current s

      fun token (s, kind) =
        (makeToken (s, start, kind), s)

      fun error (s: t, message) =
        ({kind = T.Error, lexeme = message, line = #line s}, s)

      fun match (s, expected, ok, fail) =
        case getc s of
          SOME (c, s') =>
            if c = expected then token (s', ok) else token (s, fail)
        | _ => token (s, fail)

      fun slashOrComment s =
        case getc s of
          SOME (#"/", s') => scan (dropl (fn c => c <> #"\n") s')
        | _ => token (s, T.Slash)

      fun string s =
        case getc s of
          SOME (#"\"", s') => token (s', T.String)
        | SOME (_, s') => string s'
        | NONE => error (s, "Unterminated string.")

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
        in
          token (s, T.Number)
        end

      fun identifier s =
        let
          val s = dropl (fn c => Char.isAlphaNum c orelse c = #"_") s
          val id = String.substring (#source s, start, #current s - start)
        in
          token (s, identifierType id)
        end

      val scan' =
        fn (#"(", s) => token (s, T.LeftParen)
         | (#")", s) => token (s, T.RightParen)
         | (#"{", s) => token (s, T.LeftBrace)
         | (#"}", s) => token (s, T.RightBrace)
         | (#";", s) => token (s, T.Semicolon)
         | (#",", s) => token (s, T.Comma)
         | (#".", s) => token (s, T.Dot)
         | (#"-", s) => token (s, T.Minus)
         | (#"+", s) => token (s, T.Plus)
         | (#"*", s) => token (s, T.Star)
         | (#"!", s) => match (s, #"=", T.BangEqual, T.Bang)
         | (#"=", s) => match (s, #"=", T.EqualEqual, T.Equal)
         | (#"<", s) => match (s, #"=", T.LessEqual, T.Less)
         | (#">", s) => match (s, #"=", T.GreaterEqual, T.Greater)
         | (#"/", s) => slashOrComment s
         | (#" ", s) => scan s
         | (#"\r", s) => scan s
         | (#"\t", s) => scan s
         | (#"\n", s) => scan s
         | (#"\"", s) => string s
         | (c, s) =>
          if Char.isDigit c then number s
          else if Char.isAlpha c then identifier s
          else error (s, "Unexpected character.")
    in
      case getc s of
        NONE => token (s, T.Eof)
      | SOME (c, s) => scan' (c, s)
    end
end
