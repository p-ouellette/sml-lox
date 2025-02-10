structure SourceToken:
sig
  type t = {token: Token.t, lexeme: string, line: int}

  val toString: t -> string
end =
struct
  type t = {token: Token.t, lexeme: string, line: int}

  fun toString {token, lexeme, line = _} =
    Token.toString token ^ " " ^ lexeme
end
