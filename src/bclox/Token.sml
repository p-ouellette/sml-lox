structure Token:
sig
  datatype kind =
  (* single-character tokens *)
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* one or two character tokens *)
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  (* literals *)
  | Identifier
  | String
  | Number
  (* keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While

  | Error
  | Eof

  type t = {kind: kind, lexeme: string, line: int}

  val kind: t -> kind
  val lexeme: t -> string
  val line: t -> int
end =
struct
  datatype kind =
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star

  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual

  | Identifier
  | String
  | Number

  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While

  | Error
  | Eof

  type t = {kind: kind, lexeme: string, line: int}

  fun kind (token: t) = #kind token
  fun lexeme (token: t) = #lexeme token
  fun line (token: t) = #line token
end
