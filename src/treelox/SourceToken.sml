structure SourceToken:
sig
  type t = {token: Token.t, lexeme: string, line: int}
end =
struct type t = {token: Token.t, lexeme: string, line: int} end
